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
            matchID = ''.join(random.choices(string.ascii_lowercase + string.digits, k=5))
            return matchID if matchID not in self.games.keys() else generate_game_id()

        matchID = generate_game_id()
        self.games[matchID] = GM_GameEntry(gdl_rules, startclock, playclock)
        return matchID

    def get_game(self, matchID):
        self._assert_matchID(matchID)
        return self.games[matchID].game

    def get_players(self, matchID):
        self._assert_matchID(matchID)
        return self.games[matchID].players

    def add_player(self, matchID, role, player):
        self._assert_matchID(matchID)
        self.games[matchID].add_player(role, player)

    def _assert_matchID(self, matchID):
        return matchID in self.games.keys()

    def msg_start(self, matchID):
        gdl_rules = self.get_game(matchID).gdl_rules
        startclock = self.games[matchID].startclock
        playclock = self.games[matchID].playclock
        for role, playerentry in self.games[matchID].players.items():
            print(f'GM to {role}: start({matchID}, {role}, rules, {startclock}, {playclock})')
            playerentry.status = PlayerStatus.AwaitingReady
            playerentry.player.rcv_msg(self, {'type': 'start',
                                              'matchID': matchID,
                                              'role': role,
                                              'rules': gdl_rules,
                                              'startclock': startclock,
                                              'playclock': playclock})

    def msg_play(self, matchID, moves):
        for playerentry in self.games[matchID].players.values():
            playerentry.status = PlayerStatus.AwaitingAction
        for role, playerentry in self.games[matchID].players.items():
            print(f'GM to {role}: play({matchID}, {moves})')
            playerentry.player.rcv_msg(self, {'type': 'play',
                                              'matchID': matchID,
                                              'moves': moves})

    def msg_stop(self, matchID, moves):
        for role, playerentry in self.games[matchID].players.items():
            print(f'GM to {role}: stop({matchID}, {moves}) ')
            playerentry.status = PlayerStatus.AwaitingDone
            playerentry.player.rcv_msg(self, {'type': 'stop',
                                              'matchID': matchID,
                                              'moves': moves})

    def rcv_msg(self, player, msg):
        gid, ge, role, pe = self.find_entry(player)
        if msg == 'ready':
            pe.status = PlayerStatus.Ready
            if ge.all_players_are(PlayerStatus.Ready):
                self.msg_play(gid, 'nil')
        elif msg == 'done':
            pe.status = PlayerStatus.Idle
        else:
            pe.status = PlayerStatus.PassedAction
            pe.action = msg
            if ge.all_players_are(PlayerStatus.PassedAction):
                moves = ge.get_moves()
                ge.game.apply_moves(moves)
                self.msg_stop(gid, moves) if ge.game.state.is_terminal() else self.msg_play(gid, moves)

    def find_entry(self, player):
        for matchID, gameentry in self.games.items():
            for role, playerentry in gameentry.players.items():
                if playerentry.player == player:
                    return matchID, gameentry, role, playerentry
