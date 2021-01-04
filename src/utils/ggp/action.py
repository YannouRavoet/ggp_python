"""Represents an actions taken by a role in the game (=does(role, action)). Both role and action are represented as strings."""
import re


class Action:
    def __init__(self, role, action):
        self.role = re.sub(r"\s", "", role)
        self.action = re.sub(r"\s", "", action)

    @classmethod
    def from_string(cls, string):
        """Used for inference"""
        m = re.match(r'does\(([\w0-9]*),([\w\s0-9(,)]*)\)', string)
        return cls(m.group(1), m.group(2))

    def to_term(self):
        """Used for inference"""
        return f"does({self.role},{self.action})"

    def __str__(self):
        """Used for messages"""
        return str(self.action)

    def __repr__(self):
        return self.to_term()

    def __eq__(self, other):
        return self.to_term() == other.to_term()

    def __hash__(self):
        return hash(self.to_term())
