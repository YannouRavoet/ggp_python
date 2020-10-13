import enum
import random
import string

from utils.ggp_utils import Game


class PlayerStatus(enum.Enum):
    Idle = 1  # player is cleared and not engaged in this game entry.
    AwaitingReady = 2  # start message has been sent. No response has been received.
    Ready = 2  # player has responded ready to start message.
    AwaitingAction = 3  # player has not responded to play message.
    PassedAction = 4  # player has passed his choice of action.
    AwaitingDone = 5  # player has not responded to stop or abort message.


class GM_GE_PlayerEntry(object):
    def __init__(self, player):
        self.player = player
        self.status = PlayerStatus.Idle
        self.action = None


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

    def all_players_are(self, status):
        for playerentry in self.players.values():
            if playerentry.status != status:
                return False
        return True

    def get_moves(self):
        moves = []
        for playerentry in self.players.values():
            moves.append(playerentry.action)
        return moves


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
        gdl_rules = self.get_game(gameID).gdl_rules
        startclock = self.games[gameID].startclock
        playclock = self.games[gameID].playclock
        for role, playerentry in self.games[gameID].players.items():
            print(f'GM(TX): start msg to {role}')
            playerentry.status = PlayerStatus.AwaitingReady
            playerentry.player.rcv_msg(self, {'type': 'start',
                                              'gameID': gameID,
                                              'role': role,
                                              'rules': gdl_rules,
                                              'startclock': startclock,
                                              'playclock': playclock})

    def msg_play(self, gameID, moves):
        for playerentry in self.games[gameID].players.values():
            playerentry.status = PlayerStatus.AwaitingAction
        for role, playerentry in self.games[gameID].players.items():
            print(f'GM(TX): play msg to {role}')
            playerentry.player.rcv_msg(self, {'type': 'play',
                                              'gameID': gameID,
                                              'moves': moves})

    def msg_stop(self, gameID, moves):
        for role, playerentry in self.games[gameID].players.items():
            print(f'GM(TX): stop msg to {role}')
            playerentry.status = PlayerStatus.AwaitingDone
            playerentry.player.rcv_msg(self, {'type': 'stop',
                                              'gameID': gameID,
                                              'moves': moves})

    def rcv_msg(self, player, msg):
        gid, ge, role, pe = self.find_entry(player)
        if msg == 'ready':
            print(f'GM(RX): ready msg received from {role}')
            pe.status = PlayerStatus.Ready
            if ge.all_players_are(PlayerStatus.Ready):
                self.msg_play(gid, 'nil')
        elif msg == 'done':
            print(f'GM(RX): done msg received from {role}')
            pe.status = PlayerStatus.Idle
        else:
            print(f'GM(RX): {msg} msg received from {role}')
            pe.status = PlayerStatus.PassedAction
            pe.action = msg
            if ge.all_players_are(PlayerStatus.PassedAction):
                moves = ge.get_moves()
                ge.game.apply_moves(moves)
                self.msg_stop(gid, moves) if ge.game.state.is_terminal() else self.msg_play(gid, moves)

    def find_entry(self, player):
        for gameid, gameentry in self.games.items():
            for role, playerentry in gameentry.players.items():
                if playerentry.player == player:
                    return gameid, gameentry, role, playerentry
