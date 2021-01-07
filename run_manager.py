import sys
sys.path.append("/home/yannou/git/ggp_python/src")
import argparse
from gamemanager import GameManager, Player

games = {
    # GDL
    "maze": "maze.gdl",
    "mazemedium": "mazemedium.gdl",
    "mazebig": "mazebig.gdl",
    "tictactoe": "tictactoe.gdl",
    "connectfour": "connectfour.gdl",
    "blocks": "blocks.gdl",
    "blocks2p": "blocks2p.gdl",
    "bomberman": "bomberman.gdl",
    # GDL-II
    "montyhallproblem": "montyhall.gdl",
    "kriegtictactoe": "kriegtictactoe.gdl",
    "meier": "meier.gdl",
    "transit": "transit.gdl",
    "stratego": "stratego.gdl",
    # STO GDL
    "maze_stochastic": "maze_stochastic.gdl"
}

if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    DEFAULT_PORT = 5600
    DEFAULT_STARTCLOCK = 30
    DEFAULT_PLAYCLOCK = 10


    def player(s):
        try:
            host, port = s[1:-1].split(',')
            return Player(str(host), int(port))
        except Exception:
            raise argparse.ArgumentTypeError('Player list must be of form (host, port)')


    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--port', dest='port', type=int,
                        default=DEFAULT_PORT, help='port to listen to')
    parser.add_argument('-g', '--game', dest='game', type=str, required=True, choices=list(games.keys()),
                        help='the game file to be used')
    parser.add_argument('-sc', '--startclock', dest='startclock', type=int,
                        default=DEFAULT_STARTCLOCK, help='seconds between start and play messages')
    parser.add_argument('-pc', '--playclock', dest='playclock', type=int,
                        default=DEFAULT_PLAYCLOCK, help='seconds between play messages')
    parser.add_argument('-pl', '--players', dest='players', type=player, nargs='+', required=True,
                        help='players that will play the game')
    args = parser.parse_args()

    """""""""""""""
    RUN GAMEMANAGER
    """""""""""""""
    manager = GameManager(args.port)
    matchID = manager.setup_match(games[args.game], args.players, args.startclock, args.playclock)
    manager.run_match(matchID)
