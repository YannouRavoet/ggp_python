import re
from problog.logic import Term, term2list
from utils.messaging.message_type import MessageType


class Message:
    def __init__(self, messagetype, args=None):
        self.type = messagetype
        self.args = args

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
            m = re.match(r"start\((\w*), (\w*), ([\w\s|\\,?()\[\]{}<=+]*), ([0-9]*), ([0-9]*)\)", message)
            matchID = m.group(1)
            role = Term.from_string(m.group(2))
            rules = m.group(3)
            startclock = int(m.group(4))
            playclock = int(m.group(5))
            return Message(MessageType.START, args=[matchID, role, rules, startclock, playclock])
        elif message.startswith("play"):
            m = re.match(r"play\((\w*), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                jointaction = term2list(Term.from_string(m.group(2)))
                return Message(MessageType.PLAY, args=[matchID, jointaction])
            else:
                m = re.match(r"play\((\w*), (.*), (\[.*])\)", message)
                matchID = m.group(1)
                action = Term.from_string(m.group(2)) if m.group(2) != "None" else None
                percepts = term2list(Term.from_string(m.group(3)))
                return Message(MessageType.PLAY_II, args=[matchID, action, percepts])
        elif message.startswith("stop"):
            m = re.match(r"stop\((\w*), (\[.*])\)", message)
            if m is not None:
                matchID = m.group(1)
                jointaction = term2list(Term.from_string(m.group(2)))
                return Message(MessageType.STOP, args=[matchID, jointaction])
            else:
                m = re.match(r"stop\((\w*), (.*), (\[.*])\)", message)
                matchID = m.group(1)
                action = Term.from_string(m.group(2))
                percepts = term2list(Term.from_string(m.group(3)))
                return Message(MessageType.STOP_II, args=[matchID, action, percepts])
        else:
            return Message(MessageType.ACTION, args=[Term.from_string(message)])