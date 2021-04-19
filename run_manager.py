import sys
sys.path.append("/home/yannou/git/ggp_python/src")
import argparse
from gamemanager import GameManager, Player

games = {
    # GDL
    "maze": "std_maze.gdl",
    "mazemedium": "std_maze_medium.gdl",
    "mazebig": "std_maze_big.gdl",
    "tictactoe": "std_tictactoe.gdl",
    "connectfour": "std_connectfour.gdl",
    "blocks": "std_blocks.gdl",
    "blocks2p": "std_block_2p.gdl",
    "bomberman": "std_bomberman.gdl",
    "amazons": "std_amazons.gdl",
    # GDL-II
    "montyhallproblem": "ii_montyhall.gdl",
    "kriegtictactoe": "ii_kriegtictactoe.gdl",
    "meier": "ii_meier.gdl",
    "transit": "ii_transit.gdl",
    "stratego": "ii_stratego.gdl",
    "explodingbomb": "ii_explodingbomb.gdl",
    # GDL-STO
    "maze_stochastic": "sto_maze.gdl",
    "connectfour_stochastic": "sto_connectfour.gdl",
    "dicegame": "sto_dicegame.gdl",
    "amazons_stochastic": "sto_amazons.gdl",
    "maze_stochastic_guarded": "sto_maze_medium_guarded.gdl",
    # GLD-STO-II
    "kriegtictactoe_sto": "stoii_kriegtictactoe.gdl",
}

if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    DEFAULT_PORT = 5600
    DEFAULT_STARTCLOCK = 30
    DEFAULT_PLAYCLOCK = 10
    DEFAULT_ITERATIONS = 1  #number of matches to play


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
    manager.run_matches(matchID, rounds=100)
