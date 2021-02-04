from abc import ABC
import stopit
from gameplayer import GamePlayer
from utils.ggp.state import State
from utils.pretty_print import PrettyPrinterFactory, PrettyPrinter

# quick fix to pretty print correct game
GAME_FILE = 'std_maze.gdl'

class HumanPlayer(GamePlayer, ABC):
    def __init__(self, port):
        super().__init__(port)
        self.state: State = None
        self.pretty_printer: PrettyPrinter = PrettyPrinterFactory.make_printer(GAME_FILE)

    @stopit.threading_timeoutable()
    def player_start(self):
        self.state = self.simulator.initial_state()

    @stopit.threading_timeoutable()
    def player_play(self, first_round, *args, **kwargs):
        if not first_round:
            jointaction = args[0]
            self.state = self.simulator.next_state(self.state, jointaction)
        if self.pretty_printer is not None:
            self.pretty_printer.print_state(self.state)
        legal_actions = self.simulator.legal_actions(self.state, self.role)
        for i in range(len(legal_actions)):
            print(f"[{i}]={legal_actions[i].action}")
        choice = int(input())
        return legal_actions[choice]

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        jointaction = args[0]
        self.state = self.simulator.next_state(self.state, jointaction)
        return self.simulator.goal(self.state, self.role)
