import os
import random
import socket
import string
import logging
import argparse
from threading import Thread
from http.server import HTTPServer
from http.client import HTTPConnection
from utils.msg import MessageType, MessageHandler, Message
from utils.gdl import read_rules
from utils.ggp import Simulator, MatchEntry, JointAction, GameType
from utils.pretty_print import PrettyPrinterFactory


class GameManager(HTTPServer):
    def __init__(self, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.matches = dict()

    def _jointaction(self, matchID):
        jointaction = JointAction()
        for player in self.matches[matchID]['Players']:
            jointaction.set_move(player.role, player.action)
        if self._hasrandom(matchID):
            jointaction.set_move(self.matches[matchID]['Random']['Role'], self.matches[matchID]['Random']['Action'])
        return jointaction

    def _gametype(self, matchID):
        return self.matches[matchID]['MatchEntry'].gametype

    def _hasrandom(self, matchID):
        return self.matches[matchID]['Random'] is not None

    """""""""""
     MESSAGING
    """""""""""
    def send_message(self, player, msg):
        conn = HTTPConnection(player.host, player.port)
        try:
            conn.request('POST', '', str(msg))
            try:
                response = conn.getresponse().read().decode('unicode_escape')
                return Message.parse(response)
            except socket.timeout:
                logging.error('Socket timeout...')
        finally:
            conn.close()
        return None

    def handle_message(self, msg, rcvtime):
        """
        Can be used to handle player connections.
        """
        raise NotImplementedError

    def send_start(self, matchID, player):
        msg = Message(MessageType.START,
                      args=[matchID,
                            player.role,
                            self.matches[matchID]['MatchEntry'].gdlrules,
                            self.matches[matchID]['MatchEntry'].startclock,
                            self.matches[matchID]['MatchEntry'].playclock])
        self.send_message(player, msg)

    def send_play(self, matchID, player):
        if self._gametype(matchID) == GameType.GDL:
            msg = Message(MessageType.PLAY,
                          args=[matchID,
                                self._jointaction(matchID).get_actions()])
        elif self._gametype(matchID) == GameType.GDL_II:
            msg = Message(MessageType.PLAY_II,
                          args=[matchID,
                                player.action,
                                player.percepts])
        else:
            raise NotImplementedError
        response_msg = self.send_message(player, msg)
        player.action = response_msg.args[0]

    def send_stop(self, matchID, player):
        if self._gametype(matchID) == GameType.GDL:
            msg = Message(MessageType.STOP,
                          args=[matchID,
                                self._jointaction(matchID).get_actions()])
        elif self._gametype(matchID) == GameType.GDL_II:
            msg = Message(MessageType.STOP_II,
                          args=[matchID,
                                player.action,
                                player.percepts])
        else:
            raise NotImplementedError
        self.send_message(player, msg)

    """""""""""""""
      MATCHMAKING
    """""""""""""""

    # WARNING: USE OF THE SIMULATOR IS NOT THREAD-SAFE!!!
    def thread_players(self, matchID, target):
        threads = dict()
        for player in self.matches[matchID]['Players']:
            thread = Thread(target=target, args=(matchID, player,))
            thread.start()
            threads[player] = thread
        for player in self.matches[matchID]['Players']:
            threads[player].join()

    def setup_match(self, rules, players, startclock, playclock):
        simulator = Simulator(rules)
        roles = simulator.player_roles()
        assert (len(roles) == len(players))
        for role, player in dict(zip(roles, players)).items():
            player.role = role

        matchID = ''.join(random.choices(string.ascii_lowercase + string.digits, k=5))
        self.matches[matchID] = {'MatchEntry': MatchEntry(matchID, rules, startclock, playclock, simulator.get_gametype()),
                                 'Simulator': simulator,
                                 'Players': players,
                                 'State': simulator.initial_state(),
                                 'Random': simulator.random()}
        return matchID

    def run_match(self, matchID, prettyprinter):
        match = self.matches[matchID]
        # :::::: START :::::: #
        prettyprinter.print_state(match['State'])
        socket.setdefaulttimeout(match['MatchEntry'].startclock)
        self.thread_players(matchID, self.send_start)
        # :::::: PLAY :::::: #
        socket.setdefaulttimeout(match['MatchEntry'].playclock)
        while not match['Simulator'].terminal(match['State']):
            # MESSAGE PLAYERS + GET PLAYER RESPONSE ACTIONS
            self.thread_players(matchID, self.send_play)
            # CHECK PLAYER ACTIONS
            for pl in match['Players']:
                legal_actions = match['Simulator'].legal_actions(match['State'], pl.role)
                if pl.action not in legal_actions:
                    pl.action = random.choice(legal_actions)
            # CALC ENVIRONMENT ACTION
            if self._hasrandom(matchID):
                match['Random']['Action'] = random.choice(match['Simulator'].legal_actions(match['State'],
                                                                                           match['Random']['Role']))
            # CALC PERCEPTS
            if self._gametype(matchID) == GameType.GDL_II:
                for pl in match['Players']:
                    pl.percepts = match['Simulator'].percepts(match['State'],
                                                              pl.role,
                                                              self._jointaction(matchID))
            # ADVANCE STATE
            match['State'] = match['Simulator'].next_state(match['State'],
                                                           self._jointaction(matchID))
            prettyprinter.print_state(match['State'])
        # :::::: STOP :::::: #
        self.thread_players(matchID, self.send_stop)
        for player in match['Players']:
            goal_value = match['Simulator'].goal(match['State'], player.role)
            match['MatchEntry'].add_result(player.role, goal_value)


class Player:
    def __init__(self, name, host, port):
        self.name = name
        self.host = host
        self.port = port

        self.role = None
        self.action = None
        self.percepts = None

    def set_role(self, role):
        self.role = role


if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    DEFAULT_PORT = 5600
    DEFAULT_GAME = 'tictactoe.gdl'
    DEFAULT_STARTCLOCK = 30
    DEFAULT_PLAYCLOCK = 10


    def player(s):
        try:
            name, host, port = s[1:-1].split(',')
            return Player(str(name), str(host), int(port))
        except Exception:
            raise argparse.ArgumentTypeError('Player list must be of form (name, host, port)')


    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--port', dest='port', type=int, default=DEFAULT_PORT,
                        help='port to listen to')
    parser.add_argument('-g', '--game', dest='game', type=str, default=DEFAULT_GAME,
                        help='the game file to be used')
    parser.add_argument('-sc', '--startclock', dest='startclock', type=int, default=DEFAULT_STARTCLOCK,
                        help='seconds between start and play messages')
    parser.add_argument('-pc', '--playclock', dest='playclock', type=int, default=DEFAULT_PLAYCLOCK,
                        help='seconds between play messages')
    parser.add_argument('-pl', '--players', dest='players', type=player, nargs='+', default=None,
                        help='players that will play the game')
    args = parser.parse_args()

    """""""""""""""
    RUN GAMEMANAGER
    """""""""""""""
    manager = GameManager(args.port)
    gamerules = read_rules(os.path.join('../games', args.game))
    ID = manager.setup_match(gamerules, args.players, args.startclock, args.playclock)
    pp = PrettyPrinterFactory.make_printer(args.game)
    manager.run_match(ID, prettyprinter=pp)
