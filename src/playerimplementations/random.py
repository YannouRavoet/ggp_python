import random

import stopit

from gameplayer import GamePlayer


class RandomPlayer(GamePlayer):
    """LegalPlayer selects the first legal action each round."""

    def __init__(self, port):
        super().__init__(port)
        self.state = None

    @stopit.threading_timeoutable()
    def player_start(self):
        self.state = self.simulator.initial_state()

    @stopit.threading_timeoutable()
    def player_play(self, *args, **kwargs):
        jointaction = args[0]
        if len(jointaction) > 0:
            self.state = self.simulator.next_state(self.state, jointaction)
        return random.choice(self.simulator.legal_actions(self.state, self.role))

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        jointaction = args[0]
        self.state = self.simulator.next_state(self.state, jointaction)
        return self.simulator.goal(self.state, self.role)
