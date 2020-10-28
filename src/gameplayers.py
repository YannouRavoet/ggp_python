import random
from gameplayer import GamePlayer


class LegalPlayer(GamePlayer):
    def calculate_action(self):
        legal_actions = self.match['Simulator'].legal_actions(self.match['State'], self.match['Role'])
        assert(len(legal_actions))
        return legal_actions[0]


class RandomPlayer(GamePlayer):
    def calculate_action(self):
        legal_actions = self.match['Simulator'].legal_actions(self.match['State'], self.match['Role'])
        assert(len(legal_actions))
        return random.choice(legal_actions)
