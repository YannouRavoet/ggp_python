class JointAction:
    """Represents a move for each role in the game."""

    def __init__(self, actions):
        self.actions = actions  # can't use a set since this randomizes the order in which the actions are messaged

    def to_term(self):
        """Used for inference"""
        return f"[{', '.join([action.to_term() for action in self.actions])}]"

    def get_action(self, role):
        for action in self.actions:
            if action.role == role:
                return action
        return None

    def __str__(self):
        """Used for messages"""
        return f"[{', '.join([str(action) for action in self.actions])}]"

    def __repr__(self):
        return self.to_term()

    def __len__(self):
        return len(self.actions)

    def __eq__(self, other):
        if self is None or other is None:
            return False
        return frozenset(self.actions) == frozenset(other.actions)  # order of the actions shouldn't matter for equality

    def __hash__(self):
        return hash(frozenset(self.actions))
