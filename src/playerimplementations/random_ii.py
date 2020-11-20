import stopit
import random
from gameplayer import GamePlayerII
from orderedset import OrderedSet
from utils.ggp import Action, Percepts
from utils.pretty_print import PrettyPrinterFactory


class RandomPlayerII(GamePlayerII):
    """RandomPlayer keeps track of all the hypothetically true states.
    To choose an action it picks a random action from a random hypothetically true state."""
    def __init__(self, port):
        super().__init__(port)
        self.states = OrderedSet()
        self.action_hist = list()
        self.percept_hist = list()
        self.pretty_printer = PrettyPrinterFactory.make_printer("kriegtictactoe_v2.gdl")

    def update_states(self):
        # self.states = self.simulator.update_states_ii(self.states, self.action_hist[-1], self.percept_hist[-1])
        for _ in range(len(self.states)):
            state = self.states.pop(last=False)
            if self.simulator.valid_state(state, self.percept_hist[-1]):
                jointactions = self.simulator.legal_jointactions_ii(state, self.action_hist[-1], self.percept_hist[-1])
                for jointaction in jointactions:
                    self.states.add(self.simulator.next_state(state, jointaction))

    @stopit.threading_timeoutable()
    def player_start(self):
        self.states.add(self.simulator.initial_state())

    @stopit.threading_timeoutable()
    def player_play(self, first_round, *args, **kwargs):
        if not first_round:
            self.action_hist.append(Action(self.role, args[0]))
            self.percept_hist.append(Percepts(self.role, args[1]))
            self.update_states()
        print(f"{len(self.states)} states")
        return random.choice(self.simulator.legal_actions(random.choice(tuple(self.states)), self.role))


    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        self.action_hist.append(Action(self.role, args[0]))
        self.percept_hist.append(Percepts(self.role, args[1]))
        self.update_states()
        goalsum = 0
        for state in self.states:
            goalsum += self.simulator.goal(state, self.role)
        print(len(self.states))
        print(self.states)
        return goalsum/len(self.states)
