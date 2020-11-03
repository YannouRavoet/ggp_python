from problog.logic import Term, Constant
from utils.problog import ProblogEngine


class MatchEntry(object):
    def __init__(self, matchID, gdlrules, startclock, playclock):
        self.matchID = matchID
        self.gdlrules = gdlrules
        self.startclock = startclock
        self.playclock = playclock
        self.results = dict()

    def add_result(self, role, goal):
        self.results[role] = goal


class Simulator(object):
    """
    Used to make inferences with the provided set of rules.
    """
    def __init__(self, gdl_rules):
        self.gdl_rules = gdl_rules
        self.engine = ProblogEngine(gdl_rules)

    # GDL
    def player_roles(self):
        all_roles = self.engine.query(query=Term('role', None))
        return list(filter(lambda t: t != Term('random'), all_roles))

    def initial_state(self):
        facts = self.engine.query(query=Term('init', None))
        return State(facts)

    def legal_actions(self, state, role):
        return self.engine.query(Term('legal', *[role, None]), state=state)

    def terminal(self, state):
        return self.engine.query(query=Term('terminal'), state=state, return_bool=True)

    def goal(self, state, role):
        return self.engine.query(Term('goal', *[role, None]), state=state)[0]

    def next_state(self, state, actions):
        if isinstance(actions, list):
            actions = dict(zip(self.player_roles(), actions))
        for role, action in actions.items():
            state.add_action(role, action)
        new_facts = self.engine.query(Term('next',  None), state=state)
        return State(new_facts)

    # GDL-II
    def has_random(self):
        return self.engine.query(query=Term('role', Constant('random')), return_bool=True)

    def percepts(self, state, role, actions):
        pass


class State(object):
    def __init__(self, facts):
        self.facts = facts
        self.actions = dict()

    def add_action(self, role, action):
        self.facts.append(Term('does', *[role, action]))
        self.actions[role] = action

    def __repr__(self):
        return ', '.join([str(fact) for fact in self.facts])
