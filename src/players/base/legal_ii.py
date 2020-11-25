import stopit
from gameplayer import GamePlayerII
from utils.ggp.percepts import Percepts
from utils.ggp.action import Action


class LegalPlayerII(GamePlayerII):
    """LegalPlayer selects the first legal action each round."""

    def __init__(self, port):
        super().__init__(port)
        self.states = list()
        self.max_states = 50  # max number of states to keep track of
        self.action_hist = list()
        self.percept_hist = list()

    def print_states(self, terminal):
        print(f"Currently holding {len(self.states)} hypothetical {'non-' if not terminal else ''}terminal states")

    def update_states(self, terminal):
        self.states = self.simulator.update_states_ii(self.states,
                                                      self.action_hist[-1],
                                                      self.percept_hist[-1],
                                                      terminal)[:self.max_states]
        if len(self.states) == 0:
            self.states = self.simulator.create_valid_states_ii(self.action_hist,
                                                                self.percept_hist,
                                                                terminal)[:self.max_states]

    @stopit.threading_timeoutable()
    def player_start(self):
        self.states.append(self.simulator.initial_state())

    @stopit.threading_timeoutable()
    def player_play(self, first_round, *args, **kwargs):
        if not first_round:
            self.action_hist.append(Action(self.role, args[0]))
            self.percept_hist.append(Percepts(self.role, args[1]))
            self.update_states(terminal=False)
        return self.simulator.legal_actions(self.states[0], self.role)[0]

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        self.action_hist.append(Action(self.role, args[0]))
        self.percept_hist.append(Percepts(self.role, args[1]))
        self.update_states(terminal=True)
        return self.simulator.avg_goal(self.states, self.role)
