import stopit
from gameplayer import GamePlayerII
from utils.ggp.percepts import Percepts
from utils.ggp.action import Action


class LegalPlayerII(GamePlayerII):
    """LegalPlayer selects the first legal action each round."""

    def __init__(self, port):
        super().__init__(port)
        self.states = list()
        self.action_hist = list()
        self.percept_hist = list()

    def print_states(self):
        print(f"Currently holding {len(self.states)} hypothetical states")

    def update_states(self):
        self.states = self.simulator.update_states_ii(self.states, self.action_hist[-1], self.percept_hist[-1])

    @stopit.threading_timeoutable()
    def player_start(self):
        self.states.append(self.simulator.initial_state())

    @stopit.threading_timeoutable()
    def player_play(self, first_round, *args, **kwargs):
        if not first_round:
            self.action_hist.append(Action(self.role, args[0]))
            self.percept_hist.append(Percepts(self.role, args[1]))
            self.update_states()
        return self.simulator.legal_actions(self.states[0], self.role)[0]

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        self.action_hist.append(Action(self.role, args[0]))
        self.percept_hist.append(Percepts(self.role, args[1]))
        self.update_states()
        self.states = self.simulator.filter_terminal_states(self.states)
        goalsum = 0
        for state in self.states:
            goalsum += self.simulator.goal(state, self.role)
        return goalsum / len(self.states)
