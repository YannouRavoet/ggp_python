import socket
import logging
import argparse
from http.server import HTTPServer
from http.client import HTTPConnection
from utils.messsaging import Message, MessageType, MessageHandler


class GamePlayer(HTTPServer):
    def __init__(self, name, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.name = name

    def handle_message(self, msg):
        if msg.type == MessageType.START:
            pass
        elif msg.type == MessageType.PLAY:
            pass
        elif msg.type == MessageType.STOP:
            pass
        elif msg.type == MessageType.ABORT:
            pass
        else:
            raise NotImplementedError


if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    DEFAULT_NAME = 'Unnamed Game Player'
    DEFAULT_PORT = 5601

    parser = argparse.ArgumentParser()
    parser.add_argument('-n', '--name', dest='name', type=str, default=DEFAULT_NAME,
                        help='name of the game player')
    parser.add_argument('-p', '--port', dest='port', type=int, default=DEFAULT_PORT,
                        help='port to listen to')
    args = parser.parse_args()

    """""""""""""""
    RUN GAMEPLAYER
    """""""""""""""
    player = GamePlayer(args.name, args.port)
    try:
        player.serve_forever()
    except KeyboardInterrupt:
        print('Shutting down player...')
        player.socket.close()
