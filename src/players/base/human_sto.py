from abc import ABC
import stopit
from gameplayer import GamePlayerSTO
from utils.ggp.state import State
from utils.pretty_print import PrettyPrinter, PrettyPrinterFactory

# quick fix to pretty print correct game
GAME_FILE = 'sto_maze.gdl'


class HumanSTO(GamePlayerSTO, ABC):
    def __init__(self, port):
        super().__init__(port)
        self.state: State = None
        self.pretty_printer: PrettyPrinter = PrettyPrinterFactory.make_printer(GAME_FILE)

    @stopit.threading_timeoutable()
    def player_start(self):
        self.state = self.simulator.initial_state()

    @stopit.threading_timeoutable()
    def player_play(self, *args, **kwargs):
        if not self.firstRound:
            stochastic_jointaction = args[0]
            deterministic_jointaction = args[1]
            self.state = self.simulator.next_state(self.state, deterministic_jointaction)
        if self.pretty_printer is not None:
            self.pretty_printer.print_state(self.state)
        legal_actions = self.simulator.legal_actions(self.state, self.role)
        legal_action_effects = list()
        for legal_action in legal_actions:
            legal_action_effects.append(self.simulator.get_action_outcomes(self.state, legal_action))
        for i, legal_action in legal_actions.enumerate():
            print(f"[{i}]={legal_action.action} => {legal_action_effects[legal_action]}")
        choice = int(input())
        return legal_actions[choice]

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        stochastic_jointaction = args[0]
        deterministic_jointaction = args[1]
        self.state = self.simulator.next_state(self.state, deterministic_jointaction)
        return self.simulator.goal(self.state, self.role)
