from problog.logic import Term


class Action:
    def __init__(self, role=None, action=None):
        self.role = role
        self.action = action

    @classmethod
    def from_term(cls, term):
        return cls(term.args[0], term.args[1])

    def to_term(self):
        return Term('does', *[self.role, self.action])

    def __repr__(self):
        return str(self.action)

    def __eq__(self, other):
        return self.to_term() == other.to_term()

    def __hash__(self):
        return hash(self.to_term())
