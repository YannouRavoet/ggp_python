from copy import deepcopy
from problog.logic import term2list, list2term


class State:
    """Represents the state of a game."""

    def __init__(self, facts):
        self.facts = facts

    def with_jointaction(self, jointaction):
        new_facts = deepcopy(self.facts)
        return State(new_facts + [action.to_term() for action in jointaction.actions])

    def sort(self):
        self.facts = sorted(self.facts, key=lambda t: str(t))
        return self

    def __repr__(self):
        return ', '.join([str(fact) for fact in self.facts])

    @classmethod
    def from_term(cls, term):
        return cls(term2list(term))

    def to_term(self):
        return list2term(self.facts)