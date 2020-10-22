import enum
import random
import string

from utils.ggp_utils import Game, make_move_term
from problog.logic import Term

class PlayerStatus(enum.Enum):
    Idle = 1  # player is cleared and not engaged in this game entry.
    AwaitingReady = 2  # start message has been sent. No response has been received.
    Ready = 3  # player has responded ready to start message.
    AwaitingAction = 4  # player has not responded to play message.
    PassedAction = 5  # player has passed his choice of action.
    AwaitingDone = 6  # player has not responded to stop or abort message.


class GM_ME_PlayerEntry(object):
    def __init__(self, player, role):
        self.player = player
        self.role = role
        self.status = PlayerStatus.Idle
        self.move = None
        self.percepts = None


class GM_MatchEntry(object):
    def __init__(self, gdl_rules, startclock, playclock):
        self.game = Game(gdl_rules)
        self.startclock = startclock
        self.playclock = playclock

        self.players = list()

    def add_player(self, role, player):
        assert (role in self.game.roles)
        assert (role not in map(lambda pe: pe.role, self.players))
        self.players.append(GM_ME_PlayerEntry(player, role))

    def all_players_are(self, status):
        for pe in self.players:
            if pe.status != status:
                return False
        return True


class GameManager_GDL(object):
    def __init__(self):
        self.matches = dict()

    """""""""""""""""""""""""""""""""
        HANDLE GAMES AND PLAYERS
    """""""""""""""""""""""""""""""""

    def add_game(self, gdl_rules, startclock, playclock):
        def generate_game_id():
            matchID = ''.join(random.choices(string.ascii_lowercase + string.digits, k=5))
            return matchID if matchID not in self.matches.keys() else generate_game_id()

        matchID = generate_game_id()
        self.matches[matchID] = GM_MatchEntry(gdl_rules, startclock, playclock)
        return matchID

    def get_game(self, matchID):
        assert (matchID in self.matches.keys())
        return self.matches[matchID].game

    def add_player(self, matchID, role, player):
        assert (matchID in self.matches.keys())
        self.matches[matchID].add_player(role, player)

    """""""""""""""""""""""""""""""""
        HANDLE OUTGOING MESSAGES
    """""""""""""""""""""""""""""""""

    # (START <MATCHID> <ROLE> <DESCRIPTION> <STARTCLOCK> <PLAYCLOCK>)
    def sendmsg_start(self, matchID):
        # TODO: once player and manager run on separate threads, move this to other for loop
        for pe in self.matches[matchID].players:
            pe.status = PlayerStatus.AwaitingReady

        gdl_rules = self.get_game(matchID).gdl_rules
        startclock = self.matches[matchID].startclock
        playclock = self.matches[matchID].playclock
        for pe in self.matches[matchID].players:
            print(f'GM to {pe.role}: start({matchID}, {pe.role}, rules, {startclock}, {playclock})')
            pe.player.rcv_msg(self, {'type': 'start',
                                     'matchID': matchID,
                                     'role': pe.role,
                                     'rules': gdl_rules,
                                     'startclock': startclock,
                                     'playclock': playclock})

    # (PLAY <MATCHID> <JOINTMOVES>)
    def sendmsg_play(self, matchID):
        # TODO: once player and manager run on separate threads, move this to other for loop
        for pe in self.matches[matchID].players:
            pe.status = PlayerStatus.AwaitingAction

        joint_moves = list(
            filter(lambda move: move is not None, map(lambda p: p.move, self.matches[matchID].players)))
        print(f'GM to players: play({matchID}, {joint_moves})')
        for pe in self.matches[matchID].players:
            pe.player.rcv_msg(self, {'type': 'play',
                                     'matchID': matchID,
                                     'moves': joint_moves})

    # (STOP <MATCHID> <JOINTMOVES>)
    def sendmsg_stop(self, matchID):
        # TODO: once player and manager run on separate threads, move this to other for loop
        for pe in self.matches[matchID].players:
            pe.status = PlayerStatus.AwaitingDone

        joint_moves = list(
            filter(lambda move: move is not None, map(lambda pe: pe.move, self.matches[matchID].players)))

        print(f'GM to players: stop({matchID}, {joint_moves})')
        for pe in self.matches[matchID].players:
            pe.player.rcv_msg(self, {'type': 'stop',
                                     'matchID': matchID,
                                     'moves': joint_moves})

    """""""""""""""""""""""""""""""""
        HANDLE INCOMING MESSAGES
    """""""""""""""""""""""""""""""""
    def handle_moves(self, matchID):
        match_entry = self.matches[matchID]
        moves = [make_move_term(pe.role, pe.move) for pe in match_entry.players]
        match_entry.game.extend_state_with_facts(moves)
        match_entry.game.calc_next_state()
        self.sendmsg_stop(matchID) if match_entry.game.state.is_terminal() else self.sendmsg_play(matchID)

    def rcv_msg(self, player, msg):
        def find_entry():
            for mid, me in self.matches.items():
                for pe in me.players:
                    if pe.player == player:
                        return mid, me, pe

        matchID, match_entry, player_entry = find_entry()
        if msg == 'ready':
            player_entry.status = PlayerStatus.Ready
            if match_entry.all_players_are(PlayerStatus.Ready):
                self.sendmsg_play(matchID)
        elif msg == 'done':
            player_entry.status = PlayerStatus.Idle
        else:
            player_entry.status = PlayerStatus.PassedAction
            player_entry.move = msg
            if match_entry.all_players_are(PlayerStatus.PassedAction):
                # Replace illegal moves with legal moves
                for pe in match_entry.players:
                    if not match_entry.game.state.engine.query_bool(Term('legal',*[pe.role, pe.move])):
                        pe.move = random.choice(match_entry.game.state.get_legal_actions(pe.role))
                self.handle_moves(matchID)


class GameManager_GDLII(GameManager_GDL):
    """""""""""""""""""""""""""""""""
        HANDLE INCOMING MESSAGES
    """""""""""""""""""""""""""""""""
    def handle_moves(self, matchID):
        match_entry = self.matches[matchID]
        moves = [make_move_term(pe.role, pe.move) for pe in match_entry.players]
        match_entry.game.extend_state_with_facts(moves)

        percepts = list()
        for player_entry in match_entry.players:
            player_entry.percepts = match_entry.game.state.get_percepts(player_entry.role)
            percepts.extend(player_entry.percepts)
        match_entry.game.extend_state_with_facts(percepts)  # The Game Manager sees everything
        match_entry.game.calc_next_state()
        self.sendmsg_stop(matchID) if match_entry.game.state.is_terminal() else self.sendmsg_play(matchID)

    """""""""""""""""""""""""""""""""
        HANDLE OUTGOING MESSAGES
    """""""""""""""""""""""""""""""""
    # (PLAY <MATCHID> <TURN> <LASTMOVE> <PERCEPTS>)
    def sendmsg_play(self, matchID):
        match_entry = self.matches[matchID]

        # TODO: once player and manager run on separate threads, move this to other for loop
        for player_entry in match_entry.players:
            player_entry.status = PlayerStatus.AwaitingAction

        turn = match_entry.game.turn
        for player_entry in match_entry.players:
            print(f'GM to {player_entry.role}: play({matchID}, {turn}, {player_entry.move}, {player_entry.percepts})')
            player_entry.player.rcv_msg(self, {'type': 'play',
                                               'matchID': matchID,
                                               'turn': turn,
                                               'moves': [player_entry.move],
                                               'percepts': player_entry.percepts})

    # (STOP <MATCHID> <TURN> <LASTMOVE> <PERCEPTS>)
    def sendmsg_stop(self, matchID):
        match_entry = self.matches[matchID]
        # TODO: once player and manager run on separate threads, move this to other for loop
        for player_entry in match_entry.players:
            player_entry.status = PlayerStatus.AwaitingDone

        turn = match_entry.game.turn
        for player_entry in match_entry.players:
            print(f'GM to {player_entry.role}: stop({matchID}, {turn}, {player_entry.move}, {player_entry.percepts}) ')
            player_entry.player.rcv_msg(self, {'type': 'stop',
                                               'matchID': matchID,
                                               'turn': {turn},
                                               'moves': [player_entry.move],
                                               'percepts': player_entry.percepts})
