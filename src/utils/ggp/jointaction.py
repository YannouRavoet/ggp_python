from orderedset import OrderedSet
from problog.logic import term2list, list2term
from utils.ggp.action import Action


class JointAction:
    """Represents a move for each role in the game."""

    def __init__(self, actions=None):
        self.actions = OrderedSet(actions) if actions is not None else OrderedSet()

    @classmethod
    def from_term(cls, term):
        actions = [Action.from_term(action_term) for action_term in term2list(term)]
        return cls(actions)

    def to_term(self):
        return list2term([action.to_term() for action in self.actions])

    def add_action(self, action):
        if action is not None:
            self.actions.add(action)

    def get_action(self, role):
        for action in self.actions:
            if action.role == role:
                return action
        return None

    def __repr__(self):
        return str([action.action for action in self.actions])

    def __len__(self):
        return len(self.actions)

    def __eq__(self, other):
        return self.actions == other.actions

    def __hash__(self):
        return hash(frozenset(self.actions))