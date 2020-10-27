import re
import logging
import traceback
from enum import Enum
from http.server import BaseHTTPRequestHandler


class MessageType(Enum):
    """""""""""""""""""""
    GAME MANAGER MESSAGES
    """""""""""""""""""""
    START = 0
    PLAY = 1
    STOP = 2
    II_PLAY = 3
    II_STOP = 4
    """""""""""""""""""""
    GAME PLAYER MESSAGES
    """""""""""""""""""""
    READY = 100
    ACTION = 101
    DONE = 102


class Message:
    def __init__(self, messagetype, args=None):
        self.type = messagetype
        self.args = args

    def __repr__(self):
        return str(self)

    def __str__(self):
        if self.type == MessageType.START:
            return f"start({', '.join([str(arg) for arg in self.args])})"
        elif self.type == MessageType.PLAY or self.type == MessageType.II_PLAY:
            return f"play({', '.join([str(arg) for arg in self.args])})"
        elif self.type == MessageType.STOP or self.type == MessageType.II_STOP:
            return f"stop({', '.join([str(arg) for arg in self.args])})"
        elif self.type == MessageType.READY:
            return "ready"
        elif self.type == MessageType.ACTION:
            return str(self.args[0])
        elif self.type == MessageType.DONE:
            return "done"

    @staticmethod
    def parse(message):
        # TODO: discern between GDL and GDL-II messages for play and stop (if m is None)
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
            return Message(MessageType.START,
                           args=[m.group(1), m.group(2), m.group(3), int(m.group(4)), int(m.group(5))])
        elif message.startswith("play"):
            m = re.match(r"play\((\w*), ([\w\s|\\,?()\[\]{}<=+]*)\)", message)
            return Message(MessageType.PLAY,
                           args=[m.group(1), m.group(2)])
        elif message.startswith("stop"):
            m = re.match(r"stop\((\w*), ([\w\s|\\,?()\[\]{}<=+]*)\)", message)
            return Message(MessageType.STOP,
                           args=[m.group(1), m.group(2)])
        else:
            return Message(MessageType.ACTION, [message])


class MessageHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        try:
            length = int(self.headers['content-length'])
            msg = self.rfile.read(length).decode('unicode_escape')
            print(f"received {msg}")
            msg = Message.parse(msg)
            response = self.server.handle_message(msg)
            if response is not None:
                self.respond(str(response))

        except Exception as e:
            logging.error('Error: ' + str(e) + '\n')
            traceback.print_exc()

    def respond(self, text):
        self.send_response(200, 'OK')
        if text is not None:
            self.send_header('Content-type', 'text/acl')
            self.send_header('Content-length', str(len(text)))
            self.end_headers()
            self.wfile.write(text.encode('utf-8'))
            print(f"sent {text}")


