import os
import random
import socket
import string
from threading import Thread
from http.server import HTTPServer
from http.client import HTTPConnection

from utils.gdl import read_rules
from utils.msg import MessageType, MessageHandler, Message
from utils.ggp import Simulator, JointAction
from utils.match_info import GameType, MatchInfo
from utils.pretty_print import PrettyPrinterFactory


class Player:
    def __init__(self, host, port):
        self.host = host
        self.port = port

        self.role = None
        self.action = None
        self.percepts = list()


class Match(object):
    def __init__(self, matchInfo, simulator, players, state_printer):
        self.matchInfo = matchInfo
        self.simulator = simulator
        self.players = players
        self.state = simulator.initial_state()
        self.type = simulator.get_gametype()
        self.random = simulator.random()

        self.printer = state_printer
        self.printer.print_state(self.state)

    def advance_state(self):
        self.state = self.simulator.next_state(self.state, self.jointaction())
        self.printer.print_state(self.state)

    def jointaction(self):
        jointaction = JointAction()
        for player in self.players:
            jointaction.set_move(player.role, player.action)
        if self.has_random():
            jointaction.set_move(self.random.role, self.random.action)
        return jointaction

    def check_legality_of_player_actions(self):
        for pl in self.players:
            legal_actions = self.simulator.legal_actions(self.state, pl.role)
            if pl.action not in legal_actions:
                pl.action = random.choice(legal_actions)

    def save_results(self):
        for pl in self.players:
            goal = self.simulator.goal(self.state, pl.role)
            self.matchInfo.add_result(pl.role, goal)

    def has_random(self):
        return self.random is not None

    def set_random_action(self):
        self.random.action = random.choice(self.simulator.legal_actions(self.state, self.random.role))

    def set_percepts(self):
        for pl in self.players:
            pl.percepts = self.simulator.percepts(self.state, pl.role, self.jointaction())


class GameManager(HTTPServer):
    def __init__(self, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.matches = dict()

    """""""""""
     MESSAGING
    """""""""""

    @staticmethod
    def send_message(player, msg):
        conn = HTTPConnection(player.host, player.port)
        try:
            conn.request('POST', '', str(msg))
            try:
                response = conn.getresponse().read().decode('unicode_escape')
                return Message.parse(response)
            except socket.timeout:
                print('Socket timeout...')
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
                            self.matches[matchID].matchInfo.gdlrules,
                            self.matches[matchID].matchInfo.startclock,
                            self.matches[matchID].matchInfo.playclock])
        self.send_message(player, msg)

    def send_play(self, matchID, player):
        if self.matches[matchID].type == GameType.GDL:
            msg = Message(MessageType.PLAY, args=[matchID, self.matches[matchID].jointaction()])
        elif self.matches[matchID].type == GameType.GDL_II:
            msg = Message(MessageType.PLAY_II, args=[matchID, player.action, player.percepts])
        else:
            raise NotImplementedError
        response_msg = self.send_message(player, msg)
        player.action = response_msg.args[0]

    def send_stop(self, matchID, player):
        if self.matches[matchID].type == GameType.GDL:
            msg = Message(MessageType.STOP, args=[matchID, self.matches[matchID].jointaction()])
        elif self.matches[matchID].type == GameType.GDL_II:
            msg = Message(MessageType.STOP_II, args=[matchID, player.action, player.percepts])
        else:
            raise NotImplementedError
        self.send_message(player, msg)

    """""""""""""""
      MATCHMAKING
    """""""""""""""

    # WARNING: USE OF THE SIMULATOR IS NOT THREAD-SAFE!!!
    def thread_players(self, matchID, target):
        threads = dict()
        for player in self.matches[matchID].players:
            thread = Thread(target=target, args=(matchID, player,))
            thread.start()
            threads[player] = thread
        for player in self.matches[matchID].players:
            threads[player].join()

    def setup_match(self, game, players, startclock, playclock):
        gdl_rules = read_rules(os.path.join('games', game))
        simulator = Simulator(gdl_rules)
        roles = simulator.player_roles()
        assert (len(roles) == len(players))
        for i, role in enumerate(roles):
            players[i].role = role

        matchID = ''.join(random.choices(string.ascii_lowercase + string.digits, k=5))
        state_printer = PrettyPrinterFactory.make_printer(game)
        matchInfo = MatchInfo(matchID, gdl_rules, startclock, playclock)
        self.matches[matchID] = Match(matchInfo, simulator, players, state_printer)
        return matchID

    def run_match(self, matchID):
        # :::::: START :::::: #
        match = self.matches[matchID]
        socket.setdefaulttimeout(match.matchInfo.startclock)
        self.thread_players(matchID, self.send_start)

        # :::::: PLAY :::::: #
        socket.setdefaulttimeout(match.matchInfo.playclock)
        while not match.simulator.terminal(match.state):
            # MESSAGE PLAYERS + GET PLAYER RESPONSE ACTIONS
            self.thread_players(matchID, self.send_play)
            match.check_legality_of_player_actions()

            # CALC ENVIRONMENT ACTION
            if match.type == GameType.GDL_II:
                if match.has_random():
                    match.set_random_action()
                match.set_percepts()

            # ADVANCE STATE
            match.advance_state()

        # :::::: STOP :::::: #
        self.thread_players(matchID, self.send_stop)
        match.save_results()
