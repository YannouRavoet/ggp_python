import os
import random
import socket
import string
from threading import Thread
from http.server import HTTPServer
from http.client import HTTPConnection
from typing import List, Dict

from utils.gdl_parser import read_rules
from utils.ggp.state import State
from utils.messaging.message import Message
from utils.messaging.message_type import MessageType
from utils.messaging.message_handler import MessageHandler
from utils.ggp.action import Action
from utils.ggp.inferenceinterface import InferenceInterface
from utils.ggp.jointaction import JointAction
from utils.match_info import MatchInfo
from utils.pretty_print import PrettyPrinterFactory, PrettyPrinter


class Player:
    def __init__(self, host, port, role=None):
        self.host = host
        self.port = port

        self.role = role
        self.action = None
        self.percepts = list()  # Only used with GDL-II
        self.outcome = None  # Only used with STO-GDL

    def reset(self):
        self.action = None
        self.percepts = list()  # Only used with GDL-II
        self.outcome = None  # Only used with STO-GDL

    def set_action(self, action_term):
        self.action = Action(self.role, action_term)

    def __str__(self):
        return f"{self.port} {self.role}"

    def __repr__(self):
        return str(self)


class Match(object):
    def __init__(self, matchInfo, simulator, players, state_printer):
        self.matchInfo: MatchInfo = matchInfo
        self.simulator: InferenceInterface = simulator
        self.players: List[Player] = players
        self.random: Player = Player(host=None, port=None, role='random')
        self.state: State = None
        self.round: int = 0

        self.printer: PrettyPrinter = state_printer

    def reset(self):
        self.state: State = self.simulator.initial_state()
        self.round: int = 0
        for player in self.players:
            player.reset()
        self.matchInfo.reset()
        #self.printer.print_state(self.state)

    def advance_state(self):
        if not self.matchInfo.settings.has_stochastic_actions:
            jointaction = self.get_jointaction()
        else:
            jointaction = self.get_outcomes()
        self.state = self.simulator.next_state(self.state, jointaction)
        self.round += 1
        #self.printer.print_state(self.state)

    def get_jointaction(self):
        actions = list()
        for pl in self.players:
            if pl.action is not None:  # empty for first round
                actions.append(pl.action)
        if self.matchInfo.settings.has_random:
            actions.append(self.random.action)
        return JointAction(actions)

    def check_legality_of_player_actions(self):
        for pl in self.players:
            legal_actions = self.simulator.legal_actions(self.state, pl.role)
            if pl.action is None or pl.action not in legal_actions:
                pl.action = random.choice(legal_actions)

    def set_random_action(self):
        """Sets the action for the random role"""
        self.random.action = random.choice(self.simulator.legal_actions(self.state, self.random.role))

    def set_percepts(self):
        """Sets the percepts for all players"""
        if self.matchInfo.settings.has_stochastic_actions:
            for pl in self.players:
                pl.percepts = self.simulator.get_percepts(self.state, self.get_outcomes(), pl.role)
        else:
            for pl in self.players:
                pl.percepts = self.simulator.get_percepts(self.state, self.get_jointaction(), pl.role)

    def set_outcomes(self):
        """Turns stochastic player actions choices into a deterministic action."""
        for pl in self.players:
            pl.outcome = self.simulator.sample_outcome(self.state, pl.action)

    def get_outcomes(self):
        outcomes = list()
        for pl in self.players:
            if pl.outcome is not None:  # empty for first round
                outcomes.append(pl.outcome)
        if self.matchInfo.settings.has_random:
            outcomes.append(self.random.action)
        return JointAction(outcomes)

    def save_results(self):
        for pl in self.players:
            goal = self.simulator.goal(self.state, pl.role)
            self.matchInfo.add_result(pl.role, goal)


class GameManager(HTTPServer):
    def __init__(self, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.matches: Dict[str, Match] = dict()
        self.simulator = None

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

    # --- START --- #
    def send_start(self, matchID, player):
        msg = Message(MessageType.START,
                      args=[matchID,
                            player.role,
                            self.matches[matchID].matchInfo.gdl_rules,
                            self.matches[matchID].matchInfo.startclock,
                            self.matches[matchID].matchInfo.playclock])
        self.send_message(player, msg)

    # --- PLAY --- #
    def send_play(self, matchID, player):
        if not (self.matches[matchID].matchInfo.settings.has_imperfect_information
                or self.matches[matchID].matchInfo.settings.has_stochastic_actions):
            msg = Message(MessageType.PLAY,
                          args=[matchID, self.matches[matchID].get_jointaction()])
        elif not self.matches[matchID].matchInfo.settings.has_stochastic_actions:
            msg = Message(MessageType.PLAY_II,
                          args=[matchID, self.matches[matchID].round, player.action, player.percepts])
        elif not self.matches[matchID].matchInfo.settings.has_imperfect_information:
            msg = Message(MessageType.PLAY_STO,
                          args=[matchID, self.matches[matchID].get_jointaction(), self.matches[matchID].get_outcomes()])
        else:
            msg = Message(MessageType.PLAY_STO_II,
                          args=[matchID, player.action, player.outcome, player.percepts])
        response_msg = self.send_message(player, msg)

        if response_msg is None:  # in case of timeouts
            player.set_action("noop")
        else:
            player.set_action(response_msg.args[0])

    # --- STOP --- #
    def send_stop(self, matchID, player):
        if not (self.matches[matchID].matchInfo.settings.has_imperfect_information
                or self.matches[matchID].matchInfo.settings.has_stochastic_actions):
            msg = Message(MessageType.STOP,
                          args=[matchID, self.matches[matchID].get_jointaction()])
        elif not self.matches[matchID].matchInfo.settings.has_stochastic_actions:
            msg = Message(MessageType.STOP_II,
                          args=[matchID, self.matches[matchID].round, player.action, player.percepts])
        elif not self.matches[matchID].matchInfo.settings.has_imperfect_information:
            msg = Message(MessageType.STOP_STO,
                          args=[matchID, self.matches[matchID].get_jointaction(),
                                self.matches[matchID].get_outcomes()])
        else:
            msg = Message(MessageType.STOP_STO_II,
                          args=[matchID, player.action, player.outcome, player.percepts])
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
        simulator = InferenceInterface(gdl_rules) if self.simulator is None else self.simulator
        self.simulator = simulator
        roles = simulator.get_player_roles()
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
        match.reset()
        socket.setdefaulttimeout(match.matchInfo.startclock)
        self.thread_players(matchID, self.send_start)

        # :::::: PLAY :::::: #
        socket.setdefaulttimeout(match.matchInfo.playclock)
        while not match.simulator.is_terminal(match.state):
            # MESSAGE PLAYERS + GET PLAYER RESPONSE ACTIONS
            self.thread_players(matchID, self.send_play)
            match.check_legality_of_player_actions()

            # HANDLE GAME SETTINGS
            if match.matchInfo.settings.has_stochastic_actions:
                match.set_outcomes()
            if match.matchInfo.settings.has_random:
                match.set_random_action()
            if match.matchInfo.settings.has_imperfect_information:
                match.set_percepts()

            # ADVANCE STATE
            match.advance_state()

        # :::::: STOP :::::: #
        self.thread_players(matchID, self.send_stop)
        match.save_results()
        return match.matchInfo

    def run_matches(self, matchID, rounds):
        results = {'tie': 0}
        for player in self.matches[matchID].players:
            results[player.role] = 0

        for r in range(1, rounds + 1):
            matchInfo = self.run_match(matchID)
            results[matchInfo.get_winner()] += 1
            print(results)
