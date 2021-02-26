class Outcomes:
    """Represents the outcomes of a jointaction."""

    def __init__(self, outcomes):
        self.outcomes = outcomes

    def sort(self):
        self.outcomes = sorted(self.outcomes, key=lambda t: str(t))
        return self

    def to_term(self):
        """Used for inference"""
        return f"[{', '.join(self.outcomes)}]"

    def __repr__(self):
        return self.to_term()
