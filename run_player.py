import sys
sys.path.append("/home/yannou/git/ggp_problog/src")
import argparse
from playerimplementations import legal, random, mcts, legal_ii, random_ii

playerclasses = {
    'legal': legal.LegalPlayer,
    'random': random.RandomPlayer,
    'mcts': mcts.MCTSPlayer,
    'legal_ii': legal_ii.LegalPlayerII,
    'random_ii': random_ii.RandomPlayerII
}

if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    DEFAULT_PORT = 5601

    parser = argparse.ArgumentParser()
    parser.add_argument('-c', '--playerclass', dest='playerclass', type=str, required=True, choices=list(playerclasses.keys()),
                        help='player class to implement')
    parser.add_argument('-p', '--port', dest='port', type=int, default=DEFAULT_PORT,
                        help='port to listen to')
    args = parser.parse_args()

    """""""""""""""
    RUN GAMEPLAYER
    """""""""""""""
    player = playerclasses[args.playerclass](args.port)
    try:
        player.serve_forever()
    except KeyboardInterrupt:
        print('Shutting down player...')
        player.socket.close()
