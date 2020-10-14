from utils.gdl_parsing import read_rules, parse_rules_to_string
from game_manager import GameManager_GDL, GameManager_GDLII
from game_player import LegalPlayer, RandomPlayer, HumanPlayer


if __name__ == "__main__":
    game_rules = read_rules('../games/kriegtictactoe.gdl')
    with open('../games/kriegtictactoe.pl','w') as f:
        f.write(parse_rules_to_string(game_rules))

    gm = GameManager_GDLII()
    gameID = gm.add_game(game_rules, 600, 30)
    game = gm.get_game(gameID)
    roles = game.roles
    base_propositions = game.get_base()
    all_actions_white = game.get_actions(roles[0])
    all_actions_black = game.get_actions(roles[1])

    player0 = RandomPlayer()
    player1 = RandomPlayer()
    gm.add_player(gameID, roles[0], player0)
    gm.add_player(gameID, roles[1], player1)

    gm.msg_start(gameID)







