from utils.gdl_parsing import read_rules, parse_rules_to_string
from ggp import GameManager


if __name__ == "__main__":
    game_name = 'tictactoe'
    game_rules = read_rules('../games/tictactoe.gdl')
    with open('../games/tictactoe.pl','w') as f:
        f.write(parse_rules_to_string(game_rules))

    gm = GameManager()
    game = gm.add_game(game_name, game_rules, 600, 30)
    roles = game.roles
    base_propositions = game.base
    all_actions_white = game.get_actions(roles[0])
    all_actions_black = game.get_actions(roles[1])

    state0 = game.state
    legal_white = state0.get_legal_actions(roles[0])
    legal_black = state0.get_legal_actions(roles[1])

    state1 = state0.apply_moves([legal_white[0], legal_black[0]])
    legal_white = state1.get_legal_actions(roles[0])
    legal_black = state1.get_legal_actions(roles[1])

    state2 = state1.apply_moves([legal_white[0], legal_black[0]])
    legal_white = state2.get_legal_actions(roles[0])
    legal_black = state2.get_legal_actions(roles[1])
