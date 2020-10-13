import random
import string
import enum

from utils.ggp_utils import Game


class PlayerStatus(enum.Enum):
    AwaitingStart = 1       # player is cleared and awaiting the start of a new match
    ReadyForPlay = 2        # player responded ready to the start message = is ready for new play message
    CalculatingPlay = 3     # player has not responded to play message yet
    Finishing = 4           # player has not responded to stop or abort message yet

class GM_GE_PlayerEntry(object):
    def __init__(self, player):
        self.player = player
        self.status = PlayerStatus.AwaitingStart


class GM_GameEntry(object):
    def __init__(self, gdl_rules, startclock, playclock):
        self.game = Game(gdl_rules)
        self.startclock = startclock
        self.playclock = playclock

        self.players = dict()

    def add_player(self, role, player):
        assert (role in self.game.roles)
        assert (role not in self.players.keys())
        self.players[role] = GM_GE_PlayerEntry(player)

    def set_player_status(self, role, new_status):
        self.players[role].status = new_status


class GameManagerGDL(object):
    def __init__(self):
        self.games = dict()

    def add_game(self, gdl_rules, startclock, playclock):
        def generate_game_id():
            gameId = ''.join(random.choices(string.ascii_lowercase + string.digits, k=5))
            return gameId if gameId not in self.games.keys() else generate_game_id()

        gameID = generate_game_id()
        self.games[gameID] = GM_GameEntry(gdl_rules, startclock, playclock)
        return gameID

    def get_game(self, gameID):
        self._assert_gameID(gameID)
        return self.games[gameID].game

    def get_players(self, gameID):
        self._assert_gameID(gameID)
        return self.games[gameID].players

    def add_player(self, gameID, role, player):
        self._assert_gameID(gameID)
        self.games[gameID].add_player(role, player)

    def _assert_gameID(self, gameID):
        return gameID in self.games.keys()

    def msg_start(self, gameID):
        game_rules = self.get_game(gameID).gdl_rules
        startclock = self.games[gameID].startclock
        playclock = self.games[gameID].playclock
        for role, player in self.games[gameID]['players'].items():
            player.rcv_msg(' '.join(['START', gameID, str(role), game_rules, startclock, playclock]))

    def msg_play(self, gameID, moves):
        for role, player in self.games[gameID]['players'].items():
            player.rcv_msg(' '.join(['PLAY', gameID, moves]))

    def rcv_msg(self, msg):
        pass


