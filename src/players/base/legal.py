import stopit
from gameplayer import GamePlayer


class LegalPlayer(GamePlayer):
    """LegalPlayer selects the first legal action each round."""
    def __init__(self, port):
        super().__init__(port)
        self.state = None

    @stopit.threading_timeoutable()
    def player_start(self):
        self.state = self.simulator.initial_state()

    @stopit.threading_timeoutable()
    def player_play(self, first_round, *args, **kwargs):
        if not first_round:
            jointaction = args[0]
            self.state = self.simulator.next_state(self.state, jointaction)
        return self.simulator.legal_actions(self.state, self.role)[0]

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        jointaction = args[0]
        self.state = self.simulator.next_state(self.state, jointaction)
        return self.simulator.goal(self.state, self.role)
