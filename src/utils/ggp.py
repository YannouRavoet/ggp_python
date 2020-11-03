from copy import deepcopy
from itertools import product

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
        print(f"new result for {role}: {goal}")


class Simulator(object):
    """
    Used to make inferences with the provided set of rules.
    """
    def __init__(self, gdl_rules):
        self.gdl_rules = gdl_rules
        self.engine = ProblogEngine(gdl_rules)

    # GDL
    def roles(self):
        return self.engine.query(query=Term('role', None))

    def player_roles(self):
        return list(filter(lambda t: t != Term('random'), self.roles()))

    def initial_state(self):
        facts = self.engine.query(query=Term('init', None))
        return State(facts)

    def legal_actions(self, state, role):
        return self.engine.query(Term('legal', *[role, None]), state=state)

    def legal_jointaction_permutations(self, state):
        legal_actions = dict()
        for role in self.roles():
            legal_actions[role] = self.legal_actions(state, role)
        permutations = [list(action) for action in product(*legal_actions.values())]
        return [JointAction(list(legal_actions.keys()), perm) for perm in permutations]

    def terminal(self, state):
        return self.engine.query(query=Term('terminal'), state=state, return_bool=True)

    def goal(self, state, role):
        values = self.engine.query(Term('goal', *[role, None]), state=state)
        return int(values[0])

    def next_state(self, state, jointactions):
        state_actions = state.with_actions(jointactions)
        new_facts = self.engine.query(Term('next',  None), state=state_actions)
        return State(new_facts)

    # GDL-II
    def has_random(self):
        return self.engine.query(query=Term('role', Constant('random')), return_bool=True)

    def percepts(self, state, role, actions):
        pass


class State:
    """
    Represents the state of a game.
    """
    def __init__(self, facts):
        self.facts = facts

    def with_actions(self, jointactions):
        new_facts = deepcopy(self.facts)
        for role, action in jointactions.actions.items():
            new_facts.append(Term('does', *[role, action]))
        return State(new_facts)

    def __repr__(self):
        return ', '.join([str(fact) for fact in self.facts])


class JointAction:
    """
    Represents a move for each role in the game.
    """
    def __init__(self, roles=None, actions=None):
        if roles is not None and actions is not None:
            assert(len(roles) == len(actions))
            self.actions = dict(zip(roles, actions))
        else:
            self.actions = dict()

    def set_move(self, role, action):
        self.actions[role] = action

    def get_actions(self):
        return list(self.actions.values())

    def __eq__(self, other):
        return self.actions == other.actions

    def __hash__(self):
        return hash(frozenset(self.actions))

    def __repr__(self):
        return str(self.actions)
