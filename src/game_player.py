from utils.ggp_utils import Game, make_move_term
import random

class Player(object):
    def __init__(self):
        self.role = None
        self.game = None
        self.startclock = -1
        self.playclock = -1

    def apply_moves_percepts(self, msg):
        moves = list(filter(lambda m: m is not None, msg['moves']))
        if len(moves) == len(self.game.roles):  # GDL
            self.game.extend_state_with_moves([make_move_term(role, move) for role, move in zip(self.game.roles, moves)])
            self.game.calc_next_state()
        elif len(moves) == 1:   # GDL-II
            self.game.extend_state_with_moves([make_move_term(self.role, moves[0])])
            self.game.extend_state_with_facts(msg['percepts'])
            self.game.calc_next_state()
        elif len(moves) > 0:
            NotImplementedError('A joint move set with less moves than roles is not supported.')

    def choose_action(self, gamemanager):
        pass
    """""""""""""""""""""""""""""""""
        HANDLE INCOMING MESSAGES
    """""""""""""""""""""""""""""""""
    def rcv_msg(self, gamemanager, msg):
        if msg['type'] == 'start':
            self.handle_start(gamemanager, msg)
        elif msg['type'] == 'play':
            self.handle_play(gamemanager, msg)
        elif msg['type'] == 'stop':
            self.handle_stop(gamemanager, msg)
        else:
            raise AttributeError("unsupported message")

    def handle_start(self, gamemanager, msg):
        self.role = msg['role']
        self.game = Game(msg['rules'])
        self.startclock = msg['startclock']
        self.playclock = msg['playclock']
        gamemanager.rcv_msg(self, 'ready')

    def handle_play(self, gamemanager, msg):
        self.apply_moves_percepts(msg)
        self.choose_action(gamemanager)

    def handle_stop(self, gamemanager, msg):
        self.apply_moves_percepts(msg)
        print(f'{self.role} reward: {self.game.state.get_goal(self.role)}')

        self.__init__()
        gamemanager.rcv_msg(self, 'done')


class LegalPlayer(Player):
    def choose_action(self, gamemanager):
        chosen_action = self.game.state.get_legal_actions(self.role)[0]
        gamemanager.rcv_msg(self, chosen_action)


class RandomPlayer(Player):
    def choose_action(self, gamemanager):
        chosen_action = random.choice(self.game.state.get_legal_actions(self.role))
        gamemanager.rcv_msg(self, chosen_action)


class HumanPlayer(Player):
    def choose_action(self, gamemanager):
        legal_actions = self.game.state.get_legal_actions(self.role)
        [print(f'{i}: {legal_actions[i]}') for i in range(len(legal_actions))]
        index = int(input('Chose an action from the list above.'))
        gamemanager.rcv_msg(self, legal_actions[index])
