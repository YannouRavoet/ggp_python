from problog.logic import Term, Constant

from utils.problog import ProblogEngine


class MatchEntry(object):
    def __init__(self, matchID, gdlrules, startclock, playclock):
        self.matchID = matchID
        self.gdlrules = gdlrules
        self.startclock = startclock
        self.playclock = playclock


class Simulator(object):
    """
    Used to make inferences with the provided set of rules.
    """
    def __init__(self, gdl_rules):
        self.gdl_rules = gdl_rules
        self.engine = ProblogEngine(gdl_rules)

    def player_roles(self):
        all_roles = self.engine.query(query=Term('role', None))
        return list(filter(lambda t: t != Term('random'), all_roles))

    def has_random(self):
        pass

    def initial_state(self):
        facts = self.engine.query(query=Term('init', None))
        return State(facts)

    def legal_actions(self, state, role):
        pass

    def terminal(self, state):
        return self.engine.query(query=Term('terminal'), state=state, return_bool=True)

    def goal(self, state, role):
        pass

    def next_state(self, state, moves):
        pass

    def percepts(self, state, role):
        pass


class State(object):
    def __init__(self, facts):
        self.facts = facts
        self.moves = dict()

    def add_move(self, role,  move):
        self.facts.append(Term('does', *[role, move]))
        self.moves[role] = move

    def __repr__(self):
        return ', '.join([str(fact) for fact in self.facts])


class Role(Term):
    def __init__(self, role):
        super().__init__('role', [Constant(role)])
        self.role = self.args[0]

    def __repr__(self):
        super().__repr__()


class Action(Term):
    def __init__(self, role, action):
        super().__init__(functor='does', *[role.role, action])
        self.role = self.args[0]
        self.action = self.args[1]


class Percept(Term):
    def __init__(self, role, percept):
        super().__init__(functor='sees', *[role.role, percept])
        self.role = self.args[0]
        self.percept = self.args[1]
