from utils.ggp_utils import Game

class Player(object):
    def __init__(self):
        self.role = None
        self.game = None
        self.startclock = -1
        self.playclock = -1

    def rcv_msg(self, gamemanager, msg):
        if msg['type'] == 'start':
            self.handle_start(gamemanager, msg)
        elif msg['type'] == 'play':
            self.handle_play(gamemanager, msg)
        elif msg['type'] == 'stop':
            self.handle_stop(gamemanager, msg)
        elif msg['type'] == 'abort':
            self.handle_abort(gamemanager, msg)
        else:
            raise AttributeError("unsupported message")

    # (START matchID ROLE RULES STARTCLOCK PLAYCLOCK)
    def handle_start(self, gamemanager, msg):
        self.role = msg['role']
        self.game = Game(msg['rules'])
        self.startclock = msg['startclock']
        self.playclock = msg['playclock']
        gamemanager.rcv_msg(self, 'ready')

    # (PLAY matchID MOVES)
    def handle_play(self, gamemanager, msg):
        # 1. Update the state of the game based on the passed moves
        moves = msg['moves']
        if moves != 'nil':
            self.game.state = self.game.state.apply_moves(moves)
        # 2. Chose a legal action to play
        # TO BE IMPLEMENTED BY CHILD CLASS

    # (STOP matchID MOVES)
    def handle_stop(self, gamemanager, msg):
        self.game.state = self.game.state.apply_moves(msg['moves'])
        print(f'{self.role} reward: {self.game.state.get_goal(self.role)}')
        self.__init__()
        gamemanager.rcv_msg(self, 'done')

    # (ABORT matchID)
    def handle_abort(self, gamemanager, msg):
        self.__init__()
        gamemanager.rcv_msg(self, 'done')


class LegalPlayer(Player):
    def handle_play(self, gamemanager, msg):
        super().handle_play(gamemanager, msg)
        chosen_action = self.game.state.get_legal_actions(self.role)[0]
        gamemanager.rcv_msg(self, chosen_action)

import random
class RandomPlayer(Player):
    def handle_play(self, gamemanager, msg):
        super().handle_play(gamemanager, msg)
        chosen_action = random.choice(self.game.state.get_legal_actions(self.role))
        gamemanager.rcv_msg(self, chosen_action)


class HumanPlayer(Player):
    def handle_play(self, gamemanager, msg):
        super().handle_play(gamemanager, msg)
        legal_actions = self.game.state.get_legal_actions(self.role)
        [print(f'{i}: {legal_actions[i]}') for i in range(len(legal_actions))]
        index = int(input('Chose an action from the list above.'))
        gamemanager.rcv_msg(self, legal_actions[index])
