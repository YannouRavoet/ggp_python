import os
import time
import stopit
from http.server import HTTPServer
from abc import ABC, abstractmethod

from utils.ggp.action import Action
from utils.ggp.percept import Percept
from utils.ggp.percepts import Percepts
from utils.match_info import MatchInfo
from utils.ggp.inferenceinterface import InferenceInterface
from utils.messaging.message import Message
from utils.messaging.message_type import MessageType
from utils.messaging.message_handler import MessageHandler


class GamePlayer(HTTPServer, ABC):
    def __init__(self, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.matchInfo = None
        self.simulator = None
        self.role = None

        self.msg_time = None  # time at which last message was received
        self.reply_buffer = 1  # nb seconds between ClockOverException and deadline of response
        self.reply_deadline = None  # time at which the manager needs an answer

    """""""""""
     MESSAGING
    """""""""""
    def handle_message(self, msg, rcvtime):
        self.msg_time = rcvtime
        if msg.type == MessageType.START:
            return self.handle_message_start(msg.args)
        elif msg.type in [MessageType.PLAY, MessageType.PLAY_II, MessageType.PLAY_STO, MessageType.PLAY_STO_II]:
            return self.handle_message_play(msg.args)
        elif msg.type in [MessageType.STOP, MessageType.STOP_II, MessageType.STOP_STO, MessageType.STOP_STO_II]:
            return self.handle_message_stop(msg.args)
        else:
            raise NotImplementedError

    def handle_message_start(self, msg_args):
        matchid, role, gdl_rules, startclock, playclock = msg_args
        self.set_reply_deadline(startclock)
        if self.simulator is None:
            self.simulator = InferenceInterface(gdl_rules)
        else:
            self.simulator.clear_engine()
        self.matchInfo = MatchInfo(matchid, gdl_rules, startclock, playclock, self.simulator.get_gamesettings())
        self.role = role

        self.player_start(timeout=self.clock_left())
        return Message(MessageType.READY)

    def handle_message_play(self, msg_args):
        matchid, actions = msg_args
        self.set_reply_deadline(self.matchInfo.playclock)
        jointaction = self.simulator.actionlist2jointaction(actions)
        action = self.player_play(len(jointaction) == 0, jointaction, timeout=self.clock_left())
        return Message(MessageType.ACTION, [action])

    def handle_message_stop(self, msg_args):
        matchid, actions = msg_args
        self.set_reply_deadline(self.matchInfo.playclock)
        jointaction = self.simulator.actionlist2jointaction(actions)
        goal_value = self.player_stop(jointaction, timeout=self.clock_left())
        self.matchInfo.add_result(self.role, goal_value)
        return Message(MessageType.DONE)

    @staticmethod
    def clear_screen():
        """A helper method to clear the screen of any prints. For example, can be called when a round is over."""
        os.system('clear')

    """""""""
      TIMERS
    """""""""
    def set_reply_deadline(self, clock_time):
        self.reply_deadline = self.msg_time + clock_time - self.reply_buffer

    def clock_left(self):
        return self.reply_deadline - time.time()

    """""""""""""""""""""""
    PLAYER IMPLEMENTATIONS
    """""""""""""""""""""""
    @abstractmethod
    @stopit.threading_timeoutable()
    def player_start(self):
        raise NotImplementedError

    @abstractmethod
    @stopit.threading_timeoutable()
    def player_play(self, first_round=False, *args, **kwargs):
        raise NotImplementedError

    @abstractmethod
    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        raise NotImplementedError


class GamePlayerII(GamePlayer, ABC):
    def handle_message_play(self, msg_args):
        matchid, round, action, percepts = msg_args
        self.set_reply_deadline(self.matchInfo.playclock)
        action = Action(self.role, action) if action != "None" else None
        percepts = Percepts([Percept(self.role, percept) for percept in percepts])
        next_action = self.player_play(action is None, action, percepts, timeout=self.clock_left())
        return Message(MessageType.ACTION, [next_action])

    def handle_message_stop(self, msg_args):
        matchid, round, action, percepts = msg_args
        self.set_reply_deadline(self.matchInfo.playclock)
        action = Action(self.role, action)
        percepts = Percepts([Percept(self.role, percept) for percept in percepts])
        goal_value = self.player_stop(action, percepts, timeout=self.clock_left())
        self.matchInfo.add_result(self.role, goal_value)
        return Message(MessageType.DONE)


class GamePlayerSTO(GamePlayer, ABC):
    def handle_message_play(self, msg_args):
        matchid, actions, deterministic_actions = msg_args
        self.set_reply_deadline(self.matchInfo.playclock)
        jointaction = self.simulator.actionlist2jointaction(actions)
        deterministic_jointaction = self.simulator.actionlist2jointaction(deterministic_actions)
        action = self.player_play(len(jointaction) == 0, jointaction, deterministic_jointaction, timeout=self.clock_left())
        return Message(MessageType.ACTION, [action])

    def handle_message_stop(self, msg_args):
        matchid, actions, deterministic_actions = msg_args
        self.set_reply_deadline(self.matchInfo.playclock)
        jointaction = self.simulator.actionlist2jointaction(actions)
        deterministic_jointaction = self.simulator.actionlist2jointaction(deterministic_actions)
        goal_value = self.player_stop(jointaction, deterministic_jointaction, timeout=self.clock_left())
        self.matchInfo.add_result(self.role, goal_value)
        return Message(MessageType.DONE)

