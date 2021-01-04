class State:
    """Represents the state of a game."""

    def __init__(self, facts):
        self.facts = facts

    def sort(self):
        self.facts = sorted(self.facts, key=lambda t: str(t))
        return self

    def to_term(self):
        """Used for inference"""
        return f"[{', '.join(self.facts)}]"

    def __repr__(self):
        return self.to_term()
