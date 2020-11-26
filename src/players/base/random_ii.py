import stopit
import random
from gameplayer import GamePlayerII
from utils.ggp.percepts import Percepts
from utils.ggp.action import Action
from utils.pretty_print import PrettyPrinterFactory


class RandomPlayerII(GamePlayerII):
    """RandomPlayer keeps track of all the hypothetically true states.
    To choose an action it picks a random action from a random hypothetically true state."""

    def __init__(self, port):
        super().__init__(port)
        self.states = list()
        self.max_states = 50  # max number of states to keep track of
        self.action_hist = list()
        self.percept_hist = list()

    def print_states(self, terminal):
        print(f"Currently holding {len(self.states)} hypothetical {'non-' if not terminal else ''}terminal states")

    def update_states(self):
        self.states = random.choices(
            self.simulator.update_states_ii(self.states, self.action_hist[-1], self.percept_hist[-1], filter_terminal=True),
            k=self.max_states)
        if len(self.states) == 0:
            self.states = random.choices(
                self.simulator.create_valid_states_ii(self.action_hist, self.percept_hist, filter_terminal=True),
                k=self.max_states)

    @stopit.threading_timeoutable()
    def player_start(self):
        self.states.append(self.simulator.initial_state())

    @stopit.threading_timeoutable()
    def player_play(self, first_round, *args, **kwargs):
        if not first_round:
            self.action_hist.append(Action(self.role, args[0]))
            self.percept_hist.append(Percepts(self.role, args[1]))
            self.update_states()
        return random.choice(self.simulator.legal_actions(random.choice(self.states), self.role))

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        self.action_hist.append(Action(self.role, args[0]))
        self.percept_hist.append(Percepts(self.role, args[1]))
        return self.simulator.avg_goal_from_hist(self.action_hist, self.percept_hist, self.role)
