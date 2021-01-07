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
        elif self.type == MessageType.PLAY or self.type == MessageType.PLAY_II:
            return f"play({', '.join([str(arg) for arg in self.args])})"
        elif self.type == MessageType.STOP or self.type == MessageType.STOP_II:
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
        Converts a string message into a python Message object
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
            m = re.match(r"play\((\w*), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                actions = PrologEngine.string2list(m.group(2))
                return Message(MessageType.PLAY, args=[matchID, actions])
            else:
                m = re.match(r"play\((\w*), ([0-9]*), (.*), (\[.*])\)", message)
                matchID = m.group(1)
                round = m.group(2)
                action = m.group(3)
                percepts = PrologEngine.string2list(m.group(4))
                return Message(MessageType.PLAY_II, args=[matchID, round, action, percepts])
        elif message.startswith("stop"):
            m = re.match(r"stop\((\w*), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                actions = PrologEngine.string2list(m.group(2))
                return Message(MessageType.STOP, args=[matchID, actions])
            else:
                m = re.match(r"stop\((\w*), ([0-9]*), (.*), (\[.*])\)", message)
                matchID = m.group(1)
                round = m.group(2)
                action = m.group(3)
                percepts = PrologEngine.string2list(m.group(4))
                return Message(MessageType.STOP_II, args=[matchID, round, action, percepts])
        else:
            return Message(MessageType.ACTION, args=[message])
