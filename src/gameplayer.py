import sys
import socket
import logging
import argparse
from http.server import HTTPServer, BaseHTTPRequestHandler
from http.client import HTTPConnection

from utils.messsaging import Message, MessageType

class GamePlayer(HTTPServer):
    class RequestHandler(BaseHTTPRequestHandler):
        def do_POST(self):
            try:
                print(self.server)
                sys.stdout.write('received POST')
            except Exception as e:
                sys.stderr.write('Error: ' + str(e) + '\n')
                return

        def do_GET(self):
            try:
                print(self.server)
                sys.stdout.write('received GET')
            except Exception as e:
                sys.stderr.write('Error: ' + str(e) + '\n')
                return

    def __init__(self, name, port):
        HTTPServer.__init__(self, ('', port), self.RequestHandler)
        self.name = name

    def connect_to_gamemanager(self, host, port):
        conn = HTTPConnection(host, port)
        msg = Message(MessageType.ADD_PLAYER)
        try:
            conn.request('POST', '', str(msg))
        except socket.timeout:
            logging.error('Timeout...')
        return


DEFAULT_NAME = 'Unnamed Game Player'
DEFAULT_PORT = 5601

if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', '--name', action="store", help='name of the game player', dest='name', type=str, default=DEFAULT_NAME)
    parser.add_argument('-p', '--port', action="store", help='port to listen to', dest='port', type=int, default=DEFAULT_PORT)
    parser.add_argument('-gmh', '--game manager host', action="store", help='host of the game manager to connect to', dest='gmhost', type=str, default='localhost')
    parser.add_argument('-gmp', '--game manager port', action="store", help='port of the game manager to connect to', dest='gmport', type=int, default=5600)
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
