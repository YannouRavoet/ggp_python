from utils.gdl_parsing import read_rules, parse_rules_string
from utils.problog import ProblogEngine
from problog.logic import Term,Var
from ggp import GameManager


if __name__ == "__main__":
    game_name = 'tictactoe'
    game_rules = read_rules('../games/tictactoe.gdl')

    gm = GameManager()
    game = gm.add_game(game_name, game_rules, 600, 30)
    print(game.roles())
    print(game.base())
    print(game.init().legal_actions('white'))
    print(game.init().legal_actions('black'))
    print(game.actions('white'))
    print(game.actions('black'))