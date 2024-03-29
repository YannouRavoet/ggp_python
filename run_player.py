import sys

sys.path.append("/home/yannou/git/ggp_python/src")
import argparse
from players.mcts import mcts, mcts_ii, mcts_sto_closed, mcts_sto_open, mcts_stoii_open, mcts_stoii_closed
from players.base import random, random_ii, legal, legal_ii, human, human_sto

playerClasses = {
    'human': human.HumanPlayer,
    'legal': legal.LegalPlayer,
    'random': random.RandomPlayer,
    'mcts': mcts.MCTSPlayer,
    'legal_ii': legal_ii.LegalPlayerII,
    'random_ii': random_ii.RandomPlayerII,
    'mcts_ii': mcts_ii.MCTSPlayerII,
    'human_sto': human_sto.HumanSTO,
    'mcts_sto': mcts_sto_closed.MCTSPlayerSTO,
    'open_mcts_sto': mcts_sto_open.OpenMCTSPlayerSTO,
    'open_mcts_stoii': mcts_stoii_open.OpenMCTSPlayerSTOII,
    'closed_mcts_stoii': mcts_stoii_closed.MCTSPlayerSTOII
}

if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    DEFAULT_PORT = 5601

    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--port', dest='port', type=int, default=DEFAULT_PORT,
                        help='port to listen to')
    parser.add_argument('-cls', '--playerclass', dest='playerclass', type=str, required=True,
                        choices=list(playerClasses.keys()),
                        help='player class to implement')
    args = parser.parse_args()

    """""""""""""""
    RUN GAMEPLAYER
    """""""""""""""
    player = playerClasses[args.playerclass](args.port)
    try:
        player.serve_forever()
    except KeyboardInterrupt:
        print('Shutting down player...')
        player.socket.close()
