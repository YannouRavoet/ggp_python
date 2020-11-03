import re
import logging
import time
import traceback
from enum import Enum
from problog.logic import Term, term2list
from http.server import BaseHTTPRequestHandler


class MessageType(Enum):
    """""""""""""""""""""
    GAME MANAGER MESSAGES
    """""""""""""""""""""
    START = 0
    PLAY = 1
    STOP = 2
    PLAY_II = 3
    STOP_II = 4
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
            return Message(MessageType.START,
                           args=[m.group(1),
                                 Term(m.group(2)),
                                 m.group(3),
                                 int(m.group(4)),
                                 int(m.group(5))])

        elif message.startswith("play"):
            m = re.match(r"play\((\w*), (\[[\w\s|\\,?()\[\]{}<=+]*])\)", message)
            if m is None:
                # TODO: handle PLAY_II messages
                return Message(MessageType.PLAY_II)
            return Message(MessageType.PLAY,
                           args=[m.group(1),
                                 term2list(Term.from_string(m.group(2)))])

        elif message.startswith("stop"):
            m = re.match(r"stop\((\w*), ([\w\s|\\,?()\[\]{}<=+]*)\)", message)
            if m is None:
                # TODO: handle STOP_II messages
                return Message(MessageType.STOP_II)
            return Message(MessageType.STOP,
                           args=[m.group(1),
                                 term2list(Term.from_string(m.group(2)))])

        else:
            return Message(MessageType.ACTION,
                           args=[Term.from_string(message)])


class MessageHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        try:
            rcv_time = time.time()
            length = int(self.headers['content-length'])
            msg = self.rfile.read(length).decode('unicode_escape')
            print(f"<= {msg}")
            msg = Message.parse(msg)
            response = self.server.handle_message(msg, rcv_time)
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
            print(f"=> {text}")

    def log_message(self, format, *args):
        """
        Overrides default log printing to keep from crowding the output console.
        """
        pass


