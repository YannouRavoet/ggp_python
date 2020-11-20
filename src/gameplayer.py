import time
import stopit
from abc import ABC, abstractmethod
from http.server import HTTPServer
from utils.ggp import Simulator, JointAction, Action
from utils.match_info import MatchInfo
from utils.msg import MessageType, MessageHandler, Message


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
            return self.handle_start(msg.args[0], msg.args[1], msg.args[2], msg.args[3], msg.args[4])
        elif msg.type == MessageType.PLAY:
            return self.handle_play(msg.args[0], msg.args[1])
        elif msg.type == MessageType.STOP:
            return self.handle_stop(msg.args[0], msg.args[1])
        elif msg.type == MessageType.PLAY_II:
            return self.handle_play(msg.args[0], msg.args[1], msg.args[2])
        elif msg.type == MessageType.STOP_II:
            return self.handle_stop(msg.args[0], msg.args[1], msg.args[2])
        else:
            raise NotImplementedError

    def handle_start(self, matchID, role, gdl_rules, startclock, playclock):
        self.set_reply_deadline(startclock)
        self.matchInfo = MatchInfo(matchID, gdl_rules, startclock, playclock)
        self.simulator = Simulator(gdl_rules)
        self.role = role

        self.player_start(timeout=self.clock_left())
        return Message(MessageType.READY)

    def handle_play(self, matchID, actions, *args, **kwargs):
        self.set_reply_deadline(self.matchInfo.playclock)
        action = self.player_play(len(actions) == 0, actions, timeout=self.clock_left())
        return Message(MessageType.ACTION, [action])

    def handle_stop(self, matchID, actions, *args, **kwargs):
        self.set_reply_deadline(self.matchInfo.playclock)
        goal_value = self.player_stop(actions, timeout=self.clock_left())
        self.matchInfo.add_result(self.role, goal_value)
        self.__init__(self.server_port)
        return Message(MessageType.DONE)

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

    """""""""
      TIMERS
    """""""""

    def set_reply_deadline(self, clock_time):
        self.reply_deadline = self.msg_time + clock_time - self.reply_buffer

    def clock_left(self):
        return self.reply_deadline - time.time()


class GamePlayerII(GamePlayer, ABC):
    def handle_play(self, matchID, action, percepts=None, *args, **kwargs):
        self.set_reply_deadline(self.matchInfo.playclock)
        action = self.player_play(action is None, action, percepts, timeout=self.clock_left())
        return Message(MessageType.ACTION, [action])

    def handle_stop(self, matchID, action, percepts=None, *args, **kwargs):
        self.set_reply_deadline(self.matchInfo.playclock)
        goal_value = self.player_stop(action, percepts, timeout=self.clock_left())
        self.matchInfo.add_result(self.role, goal_value)
        self.__init__(self.server_port)
        return Message(MessageType.DONE)
