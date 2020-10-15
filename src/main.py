from utils.gdl_parsing import read_rules, parse_rules_to_string
from game_manager import GameManager_GDL, GameManager_GDLII
from game_player import LegalGamePlayer, RandomGamePlayer, HumanGamePlayer


if __name__ == "__main__":
    game_rules = read_rules('../games/tictactoe.gdl')
    with open('../games/tictactoe.pl','w') as f:
        f.write(parse_rules_to_string(game_rules))

    gm = GameManager_GDL()
    gameID = gm.add_game(game_rules, 600, 30)
    game = gm.get_game(gameID)
    roles = game.roles

    gm.add_player(gameID, roles[0], RandomGamePlayer())
    gm.add_player(gameID, roles[1], RandomGamePlayer())

    gm.sendmsg_start(gameID)







