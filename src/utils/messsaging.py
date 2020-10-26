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
    ABORT = 3
    """""""""""""""""""""
    GAME PLAYER MESSAGES
    """""""""""""""""""""
    READY = 4
    ACTION = 5
    DONE = 6


class Message:
    def __init__(self, messagetype, args=None):
        self.type = messagetype
        self.args = args

    def __repr__(self):
        return str(self)

    def __str__(self):
        return f"{self.type.name}({', '.join([str(arg) for arg in self.args]) if self.args is not None else ''})"

    @staticmethod
    def parse(message):
        [type, content] = message.split('(', 2)
        type = MessageType[type]
        content = content[:-1]
        if type == MessageType.ADD_PLAYER:
            name, host, port = content.split(',')
            return Message(type, [name, host, int(port)])
        return None


class MessageHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        try:
            length = int(self.headers['content-length'])
            msg = Message.parse(str(self.rfile.read(length))[2:-1])
            response = self.server.handle_message(msg)
            if response is not None:
                self.respond(response)

        except Exception as e:
            logging.error('Error: ' + str(e) + '\n')
            traceback.print_exc()
            return

    def respond(self, text):
        self.send_response(200, 'OK')
        if text is not None:
            self.send_header('Content-type', 'text/acl')
            self.send_header('Content-length', str(len(text)))
            self.end_headers()
            self.wfile.write(text.encode('utf-8'))
