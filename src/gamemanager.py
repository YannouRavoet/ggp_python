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
from utils.ggp import Simulator, MatchEntry


# TODO: Create child class for GDL-II that overwrites send_play and send_stop
class GameManager(HTTPServer):
    def __init__(self, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.match = None

    def send_message(self, role, msg):
        (_, host, port), _ = self.match['Players'][role]
        conn = HTTPConnection(host, port)
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

    def handle_message(self, msg):
        """
        Can be used to handle player connections (adding, removing)
        :param msg: msg.Message of type msg.MessageType to process
        :return: response message to sender
        """
        raise NotImplementedError

    def thread_roles(self, target):
        threads = dict()
        for role in self.match['Players']:
            thread = Thread(target=target, args=(role,))
            thread.start()
            threads[role] = thread
        for role in self.match['Players']:
            threads[role].join()

    def setup_match(self, gdl_rules, players, startclock, playclock):
        matchID = ''.join(random.choices(string.ascii_lowercase + string.digits, k=5))
        simulator = Simulator(gdl_rules)
        roles = simulator.player_roles()
        assert (len(roles) == len(players))
        actions = [None] * len(roles)
        players = dict(zip(roles, list(map(list, zip(players, actions)))))
        self.match = {'MatchEntry': MatchEntry(matchID, gdl_rules, startclock, playclock),
                      'Simulator': simulator,
                      'Players': players,
                      'State': None}

    def run_match(self):
        # START MATCH
        socket.setdefaulttimeout(self.match['MatchEntry'].startclock)
        self.thread_roles(self.send_start)
        self.match['State'] = self.match['Simulator'].initial_state()
        socket.setdefaulttimeout(self.match['MatchEntry'].playclock)
        # PLAY MATCH
        while not self.match['Simulator'].terminal(self.match['State']):
            self.thread_roles(self.send_play)
            joint_actions = list()
            for role, ((name, _, _), action) in self.match['Players'].items():
                legal_actions = self.match['Simulator'].legal_actions(self.match['State'], role)
                if action not in legal_actions:
                    action = random.choice(legal_actions)
                joint_actions.append(action)
            self.match['State'] = self.match['Simulator'].next_state(self.match['State'], joint_actions)
        # STOP MATCH
        self.thread_roles(self.send_stop)

    def send_start(self, role):
        msg = Message(MessageType.START,
                      args=[self.match['MatchEntry'].matchID,
                            role,
                            self.match['MatchEntry'].gdlrules,
                            self.match['MatchEntry'].startclock,
                            self.match['MatchEntry'].playclock])
        self.send_message(role, msg)

    def send_play(self, role):
        jointactions = [action for _, (_, action) in self.match['Players'].items()]
        msg = Message(MessageType.PLAY,
                      args=[self.match['MatchEntry'].matchID,
                            jointactions])
        response_msg = self.send_message(role, msg)
        self.match['Players'][role][1] = response_msg.args[0]

    def send_stop(self, role):
        jointactions = [action for _, (_, action) in self.match['Players'].items()]
        msg = Message(MessageType.STOP,
                      args=[self.match['MatchEntry'].matchID,
                            jointactions])
        self.send_message(role, msg)


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
            return str(name), str(host), int(port)
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
    gdl_rules = read_rules(os.path.join('../games', args.game))
    manager.setup_match(gdl_rules, args.players, args.startclock, args.playclock)
    manager.run_match()
