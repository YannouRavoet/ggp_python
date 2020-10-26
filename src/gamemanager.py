import os
import argparse
from http.server import HTTPServer
from utils.messsaging import MessageType, MessageHandler
from utils.gdl_parser import read_rules


class GameManager(HTTPServer):
    def __init__(self, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.players = None

    def handle_message(self, msg):
        if msg.type == MessageType.READY:
            pass
        elif msg.type == MessageType.ACTION:
            pass
        elif msg.type == MessageType.DONE:
            pass
        else:
            raise NotImplementedError

    def run_match(self, gdl_rules, players, startclock, playclock):
        # make match entry
        # contains: matchID, players, simulator, results
        # startup simulator
        # assign roles to players
        # send start message to all players (has to be threaded since it awaits the response of the player)
        return




if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    DEFAULT_PORT = 5600
    DEFAULT_GAME = 'tictactoe.gdl'
    DEFAULT_STARTCLOCK = 30
    DEFAULT_PLAYCLOCK = 10

    def player(s):
        try:
            host, port = s[1:-1].split(',')
            return str(host), int(port)
        except Exception:
            raise argparse.ArgumentTypeError('Player list must be of form ')

    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--port', dest='port', type=int, default=DEFAULT_PORT,
                        help='port to listen to')
    parser.add_argument('-g', '--game', dest='game', type=str, default=DEFAULT_GAME,
                        help='the game file to be used')
    parser.add_argument('-sc', '--startclock', dest='startclock', type=int, default=DEFAULT_STARTCLOCK,
                        help='seconds between start and play messages')
    parser.add_argument('-pc', '--playclock', dest='playclock', type=int, default=DEFAULT_PLAYCLOCK,
                        help='seconds between play messages')
    parser.add_argument('-pl', '--players', dest='players', type=player, nargs='+', default=None,
                        help='players that will play the game')
    args = parser.parse_args()

    """""""""""""""
    RUN GAMEMANAGER
    """""""""""""""
    manager = GameManager(args.port)
    gdl_rules = read_rules(os.path.join('../games', args.game))
    manager.run_match(gdl_rules, args.players, args.startclock, args.playclock)
