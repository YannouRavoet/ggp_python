class GameManager(object):
    def __init__(self):
        self.games = dict()
        self.gdlParser = None

    def create_game(self, gameID, gdl_file, startclock, playclock):
        self.games[gameID] = Game(gameID, gdl_file, startclock, playclock)
        return self.games[gameID]

    def end_game(self, gameID):
        del self.games[gameID]

    def game_exists(self, gameID):
        return gameID in self.games


class Game(object):
    def __init__(self, gameID, gamerules, startclock, playclock):
        self.gameID = gameID
        self.gameRules = gamerules
        self.startclock = startclock
        self.playclock = playclock
        self.players = dict()


