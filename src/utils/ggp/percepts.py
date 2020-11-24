from problog.logic import list2term, term2list, Term


class Percepts:
    """Represents the percepts of a player"""

    def __init__(self, role, percepts):
        self.role = role
        self.percepts = percepts

    @classmethod
    def from_term(cls, term):
        if term == list2term([]):
            return cls(None, list())
        role = term2list(term)[0].args[0]
        percepts = [sees_term.args[1] for sees_term in term2list(term)]
        return cls(role, percepts)

    def to_term(self):
        terms = [Term('sees', *[self.role, percept]) for percept in self.percepts]
        return list2term(terms)

    def __repr__(self):
        return str(self.percepts)

    def __eq__(self, other):
        return self.to_term() == other.to_term()

    def __hash__(self):
        return hash(self.to_term())
