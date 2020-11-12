from gameplayer import GamePlayerII


class LegalPlayerII(GamePlayerII):
    """LegalPlayer selects the first legal action each round."""
    def __init__(self, port):
        super().__init__(port)
        self.state = None
        self.action_hist = list()
        self.percept_hist = list()

    def player_start(self):
        self.state = self.simulator.initial_state()

    def player_play(self, *args, **kwargs):
        if args[0] is not None:
            self.action_hist.append(args[0])
            self.percept_hist.append(args[1])
            if self.simulator.valid_state(self.state, self.role, args[1]):
                jointaction = self.simulator.legal_jointaction_from_percepts(self.state, self.role, args[0], args[1])
                self.state = self.simulator.next_state(self.state, jointaction)
            else:
                self.state = self.simulator.create_valid_state(self.role, self.action_hist, self.percept_hist)
            return self.simulator.legal_actions(self.state, self.role)[0]
        else:
            return self.simulator.legal_actions(self.state, self.role)[0]

    def player_stop(self, *args, **kwargs):
        self.action_hist.append(args[0])
        self.percept_hist.append(args[1])
        jointaction = self.simulator.legal_jointaction_from_percepts(self.state, self.role, args[0], args[1])
        self.state = self.simulator.next_state(self.state, jointaction)
        return self.simulator.goal(self.state, self.role)