import os
import random
import socket
import string
from threading import Thread
from http.server import HTTPServer
from http.client import HTTPConnection
from typing import List, Dict

from utils.gdl import read_rules
from utils.ggp.state import State
from utils.messaging.message import Message
from utils.messaging.message_type import MessageType
from utils.messaging.message_handler import MessageHandler
from utils.ggp.action import Action
from utils.ggp.simulator import Simulator
from utils.ggp.jointaction import JointAction
from utils.match_info import MatchInfo
from utils.pretty_print import PrettyPrinterFactory, PrettyPrinter


class Player:
    def __init__(self, host, port, role=None):
        self.host = host
        self.port = port

        self.role = role
        self.action = None
        self.percepts = list()

    def set_action(self, action_term):
        self.action = Action(self.role, action_term)


class Match(object):
    def __init__(self, matchInfo, simulator, players, state_printer):
        self.matchInfo: MatchInfo = matchInfo
        self.simulator: Simulator = simulator
        self.players: List[Player] = players
        self.random: Player = Player(host=None, port=None, role='random')
        self.state: State = simulator.initial_state()
        self.round: int = 0

        self.printer: PrettyPrinter = state_printer
        self.printer.print_state(self.state)

    def advance_state(self):
        self.state = self.simulator.next_state(self.state, self.get_jointaction())
        self.round += 1
        self.printer.print_state(self.state)

    def get_jointaction(self):
        actions = list()
        for player in self.players:
            if player.action is not None:
                actions.append(player.action)
        if self.matchInfo.settings.has_random:
            actions.append(self.random.action)
        return JointAction(actions)

    def check_legality_of_player_actions(self):
        for pl in self.players:
            legal_actions = self.simulator.legal_actions(self.state, pl.role)
            if pl.action is None or pl.action not in legal_actions:
                pl.action = random.choice(legal_actions)

    def set_random_action(self):
        self.random.action = random.choice(self.simulator.legal_actions(self.state, self.random.role))

    def set_percepts(self):
        for pl in self.players:
            pl.percepts = self.simulator.get_percepts(self.state, self.get_jointaction(), pl.role)

    def set_deterministic_action(self):
        """ Turns stochastic player actions choices into a chosen deterministic action."""
        return None

    def save_results(self):
        for pl in self.players:
            goal = self.simulator.goal(self.state, pl.role)
            self.matchInfo.add_result(pl.role, goal)


class GameManager(HTTPServer):
    def __init__(self, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.matches: Dict[str, Match] = dict()

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                        MESSAGING
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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
        """ Can be used to handle player connections. """
        raise NotImplementedError

    def send_start(self, matchID, player):
        msg = Message(MessageType.START,
                      args=[matchID,
                            player.role,
                            self.matches[matchID].matchInfo.gdl_rules,
                            self.matches[matchID].matchInfo.startclock,
                            self.matches[matchID].matchInfo.playclock])
        self.send_message(player, msg)

    def send_play(self, matchID, player):
        if not self.matches[matchID].matchInfo.settings.has_imperfect_information \
                and not self.matches[matchID].matchInfo.settings.has_stochastic_actions:
            msg = Message(MessageType.PLAY, args=[matchID, self.matches[matchID].get_jointaction()])
        elif not self.matches[matchID].matchInfo.settings.has_stochastic_actions:
            msg = Message(MessageType.PLAY_II, args=[matchID, self.matches[matchID].round, player.action, player.percepts])
        else:
            raise NotImplementedError  # TODO: implement stochastic action message

        response_msg = self.send_message(player, msg)
        player.set_action(response_msg.args[0])

    def send_stop(self, matchID, player):
        if not self.matches[matchID].matchInfo.settings.has_imperfect_information \
                and not self.matches[matchID].matchInfo.settings.has_stochastic_actions:
            msg = Message(MessageType.STOP, args=[matchID, self.matches[matchID].get_jointaction()])
        elif not self.matches[matchID].matchInfo.settings.has_stochastic_actions:
            msg = Message(MessageType.STOP_II, args=[matchID, self.matches[matchID].round, player.action, player.percepts])
        else:
            raise NotImplementedError  # TODO: implement stochastic action message
        self.send_message(player, msg)

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                MATCHMAKING
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    def thread_players(self, matchID, target):
        threads = dict()
        for player in self.matches[matchID].players:
            thread = Thread(target=target, args=(matchID, player,))
            thread.start()
            threads[player] = thread
        for player in self.matches[matchID].players:
            threads[player].join()

    def setup_match(self, game_file: str, players: List[Player], startclock: int, playclock: int):
        state_printer = PrettyPrinterFactory.make_printer(game_file)
        gdl_rules = read_rules(os.path.join('games', game_file))
        simulator = Simulator(gdl_rules)
        roles = simulator.player_roles()
        assert (len(roles) == len(players))
        for i, role in enumerate(roles):
            players[i].role = role

        matchID = ''.join(random.choices(string.ascii_lowercase + string.digits, k=5))
        matchInfo = MatchInfo(matchID, gdl_rules, startclock, playclock, simulator.get_gamesettings())
        self.matches[matchID] = Match(matchInfo, simulator, players, state_printer)
        return matchID

    def run_match(self, matchID):
        # :::::: START :::::: #
        match = self.matches[matchID]
        socket.setdefaulttimeout(match.matchInfo.startclock)
        self.thread_players(matchID, self.send_start)

        # :::::: PLAY :::::: #
        socket.setdefaulttimeout(match.matchInfo.playclock)
        while not match.simulator.is_terminal(match.state):
            # MESSAGE PLAYERS + GET PLAYER RESPONSE ACTIONS
            self.thread_players(matchID, self.send_play)
            match.check_legality_of_player_actions()

            # CALC ENVIRONMENT ACTION
            if match.matchInfo.settings.has_stochastic_actions:
                match.set_deterministic_action()
            if match.matchInfo.settings.has_random:
                match.set_random_action()
            if match.matchInfo.settings.has_imperfect_information:
                match.set_percepts()

            # ADVANCE STATE
            match.advance_state()

        # :::::: STOP :::::: #
        self.thread_players(matchID, self.send_stop)
        match.save_results()
