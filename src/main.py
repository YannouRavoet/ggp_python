from utils.gdl_parsing import read_rules, parse_rules_to_string
from game_manager import GameManagerGDL,  Player


if __name__ == "__main__":

    game_rules = read_rules('../games/tictactoe.gdl')
    gm = GameManagerGDL()
    gameID = gm.add_game(game_rules, 600, 30)
    game = gm.get_game(gameID)
    roles = game.roles
    base_propositions = game.get_base()
    all_actions_white = game.get_actions(roles[0])
    all_actions_black = game.get_actions(roles[1])

    player0 = Player()
    player1 = Player()
    gm.add_player(gameID, roles[0], player0)
    gm.add_player(gameID, roles[1], player1)




