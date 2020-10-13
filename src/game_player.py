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

    # (START GAMEID ROLE RULES STARTCLOCK PLAYCLOCK)
    def handle_start(self, gamemanager, msg):
        print(f'{self.role}: start msg received')
        self.role = msg['role']
        self.game = Game(msg['rules'])
        self.startclock = msg['startclock']
        self.playclock = msg['playclock']
        gamemanager.rcv_msg(self, 'ready')

    # (PLAY GAMEID MOVES)
    def handle_play(self, gamemanager, msg):
        print(f'{self.role}: play msg received')
        # 1. Update the state of the game based on the passed moves
        moves = msg['moves']
        if moves != 'nil':
            self.game.state = self.game.state.apply_moves(moves)

    # (STOP GAMEID MOVES)
    def handle_stop(self, gamemanager, msg):
        print(f'{self.role}: stop msg received')
        self.__init__()
        gamemanager.rcv_msg(self, 'done')

    # (ABORT GAMEID)
    def handle_abort(self, gamemanager, msg):
        print(f'{self.role}: abort msg received')
        self.__init__()
        gamemanager.rcv_msg(self, 'done')


class LegalPlayer(Player):
    def handle_play(self, gamemanager, msg):
        super().handle_play(gamemanager, msg)
        chosen_action = self.game.state.get_legal_actions(self.role)[0]
        gamemanager.rcv_msg(self, chosen_action)


class RandomPlayer(Player):
    def handle_play(self, gamemanager, msg):
        pass

class HumanPlayer(Player):
    def handle_play(self, gamemanager, msg):
        pass

