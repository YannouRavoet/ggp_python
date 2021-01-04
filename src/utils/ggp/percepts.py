class Percepts:
    """Represents the percepts of a player"""

    def __init__(self, percepts):
        self.percepts = percepts

    def to_term(self):
        """Used for inference"""
        return f"[{','.join([percept.to_term() for percept in self.percepts])}]"

    def __str__(self):
        """Used for messages"""
        return f"[{','.join([str(percept) for percept in self.percepts])}]"

    def __repr__(self):
        return self.to_term()

    def __eq__(self, other):
        return frozenset(self.percepts) == frozenset(other.get_percepts)

    def __hash__(self):
        return hash(frozenset(self.percepts))
