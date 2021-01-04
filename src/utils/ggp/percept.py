import re


class Percept:
    def __init__(self, role, percept):
        self.role = re.sub(r"\s", "", role)
        self.percept = re.sub(r"\s", "", percept)

    def to_term(self):
        """Used for inference"""
        return f"sees({self.role},{self.percept})"

    @classmethod
    def from_string(cls, string):
        m = re.match(r'sees\(([\w0-9]*),([\w\s0-9(,)]*)\)', string)
        return cls(m.group(1), m.group(2))

    def __str__(self):
        """Used for messages"""
        return f"{self.percept}"

    def __repr__(self):
        return self.to_term()

    def __eq__(self, other):
        return self.to_term() == other.to_term()

    def __hash__(self):
        return hash(self.to_term())
