import re
from utils.prolog.prolog import PrologEngine
from utils.messaging.message_type import MessageType


class Message:
    def __init__(self, messagetype, args=None):
        self.type = messagetype
        self.args = args
        # args are of str/int/list<str;int> format
        # processing into Actions, JointAction, Percepts is tasked to the manager/player.

    def __repr__(self):
        return str(self)

    def __str__(self):
        if self.type == MessageType.START:
            return f"start({', '.join([str(arg) for arg in self.args])})"
        elif self.type in [MessageType.PLAY, MessageType.PLAY_II, MessageType.PLAY_STO, MessageType.PLAY_STO_II]:
            return f"play({', '.join([str(arg) for arg in self.args])})"
        elif self.type in [MessageType.STOP, MessageType.STOP_II, MessageType.STOP_STO, MessageType.STOP_STO_II]:
            return f"stop({', '.join([str(arg) for arg in self.args])})"
        elif self.type == MessageType.READY:
            return "ready"
        elif self.type == MessageType.ACTION:
            return str(self.args[0])
        elif self.type == MessageType.DONE:
            return "done"

    @staticmethod
    def parse(message):
        """
        Converts a string message into a python Message object.
        Checks for the most complex match first to prevent overlapping matches:
        e.g., 'play(6zq56, [], [])' matches r"play\((\w*), (\[.*])\)".
        :param message: string of the message to parse
        :return: Message object. Content is found in the 'args' variable in the same ordered as received in the message
        """
        if message == "ready":
            return Message(MessageType.READY)
        elif message == "done":
            return Message(MessageType.DONE)
        elif message.startswith("start"):
            m = re.match(r"start\((\w*), (\w*), ([\w\s|\\,.?()\[\]{}<>=+]*), ([0-9]*), ([0-9]*)\)", message)
            matchID = m.group(1)
            role = m.group(2)
            rules = m.group(3)
            startclock = int(m.group(4))
            playclock = int(m.group(5))
            return Message(MessageType.START, args=[matchID, role, rules, startclock, playclock])
        elif message.startswith("play"):
            # --- PLAY_STO --- #
            m = re.match(r"play\((\w*), (\[.*]), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                actions = PrologEngine.string2list(m.group(2))
                effects = PrologEngine.string2list(m.group(2))
                return Message(MessageType.PLAY_STO, args=[matchID, actions, effects])
            # --- PLAY_II --- #
            m = re.match(r"play\((\w*), ([0-9]*), (.*), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                round = m.group(2)
                action = m.group(3)
                percepts = PrologEngine.string2list(m.group(4))
                return Message(MessageType.PLAY_II, args=[matchID, round, action, percepts])
            # --- PLAY --- #
            m = re.match(r"play\((\w*), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                actions = PrologEngine.string2list(m.group(2))
                return Message(MessageType.PLAY, args=[matchID, actions])
        elif message.startswith("stop"):
            # --- STOP_STO --- #
            m = re.match(r"stop\((\w*), (\[.*]), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                actions = PrologEngine.string2list(m.group(2))
                effects = PrologEngine.string2list(m.group(2))
                return Message(MessageType.STOP_STO, args=[matchID, actions, effects])
            # --- STOP_II --- #
            m = re.match(r"stop\((\w*), ([0-9]*), (.*), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                round = m.group(2)
                action = m.group(3)
                percepts = PrologEngine.string2list(m.group(4))
                return Message(MessageType.STOP_II, args=[matchID, round, action, percepts])
            # --- STOP --- #
            m = re.match(r"stop\((\w*), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                actions = PrologEngine.string2list(m.group(2))
                return Message(MessageType.STOP, args=[matchID, actions])
        else:
            return Message(MessageType.ACTION, args=[message])
