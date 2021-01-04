import time
import stopit
from http.server import HTTPServer
from abc import ABC, abstractmethod

from utils.ggp.action import Action
from utils.ggp.percept import Percept
from utils.ggp.percepts import Percepts
from utils.match_info import MatchInfo
from utils.ggp.simulator import Simulator
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
        self.reply_buffer = 2  # nb seconds between ClockOverException and deadline of response
        self.reply_deadline = None  # time at which the manager needs an answer

    """""""""""
     MESSAGING
    """""""""""

    def handle_message(self, msg, rcvtime):
        self.msg_time = rcvtime
        if msg.type == MessageType.START:
            return self.handle_start(msg.args[0], msg.args[1], msg.args[2], msg.args[3], msg.args[4])
        elif msg.type == MessageType.PLAY:
            return self.handle_play(msg.args[0], msg.args[1])
        elif msg.type == MessageType.STOP:
            return self.handle_stop(msg.args[0], msg.args[1])
        elif msg.type == MessageType.PLAY_II:
            return self.handle_play(msg.args[0], msg.args[2], msg.args[3])  # ignores the round argument
        elif msg.type == MessageType.STOP_II:
            return self.handle_stop(msg.args[0], msg.args[2], msg.args[3])  # ignores the round argument
        else:
            raise NotImplementedError

    def handle_start(self, matchID, role, gdl_rules, startclock, playclock):
        self.set_reply_deadline(startclock)
        self.simulator = Simulator(gdl_rules)
        self.matchInfo = MatchInfo(matchID, gdl_rules, startclock, playclock, self.simulator.get_gamesettings())
        self.role = role

        self.player_start(timeout=self.clock_left())
        return Message(MessageType.READY)

    def handle_play(self, matchID, actions, *args, **kwargs):
        self.set_reply_deadline(self.matchInfo.playclock)
        jointaction = self.simulator.actions_2_jointaction(actions)
        action = self.player_play(len(jointaction) == 0, jointaction, timeout=self.clock_left())
        return Message(MessageType.ACTION, [action])

    def handle_stop(self, matchID, actions, *args, **kwargs):
        self.set_reply_deadline(self.matchInfo.playclock)
        jointaction = self.simulator.actions_2_jointaction(actions)
        goal_value = self.player_stop(jointaction, timeout=self.clock_left())
        self.matchInfo.add_result(self.role, goal_value)
        self.__init__(self.server_port)
        return Message(MessageType.DONE)

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
    def handle_play(self, matchID, action, percepts=None, *args, **kwargs):
        self.set_reply_deadline(self.matchInfo.playclock)
        action = Action(self.role, action) if action != "None" else None
        percepts = Percepts([Percept(self.role, percept) for percept in percepts])
        next_action = self.player_play(action is None, action, percepts, timeout=self.clock_left())
        return Message(MessageType.ACTION, [next_action])

    def handle_stop(self, matchID, action, percepts=None, *args, **kwargs):
        self.set_reply_deadline(self.matchInfo.playclock)
        action = Action(self.role, action)
        percepts = Percepts([Percept(self.role, percept) for percept in percepts])
        goal_value = self.player_stop(action, percepts, timeout=self.clock_left())
        self.matchInfo.add_result(self.role, goal_value)
        self.__init__(self.server_port)
        return Message(MessageType.DONE)
