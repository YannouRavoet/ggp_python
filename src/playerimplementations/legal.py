from gameplayer import GamePlayer


class LegalPlayer(GamePlayer):
    """LegalPlayer selects the first legal action each round."""
    def __init__(self, port):
        super().__init__(port)
        self.state = None

    def player_start(self):
        self.state = self.simulator.initial_state()

    def player_play(self, *args, **kwargs):
        jointaction = args[0]
        self.state = self.simulator.next_state(jointaction)
        return self.simulator.legal_actions(self.state, self.role)[0]

    def player_stop(self, *args, **kwargs):
        jointaction = args[0]
        self.state = self.simulator.next_state(jointaction)
        return self.simulator.goal(self.state, self.role)
