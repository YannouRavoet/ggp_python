from copy import deepcopy
from enum import Enum
from itertools import product
from problog.logic import Term, Constant, Var, list2term
from utils.problog import ProblogEngine


class GameType(Enum):
    GDL = 1
    GDL_II = 2


class MatchEntry(object):
    def __init__(self, matchID, gdlrules, startclock, playclock, gametype):
        self.matchID = matchID
        self.gdlrules = gdlrules
        self.startclock = startclock
        self.playclock = playclock
        self.results = dict()
        self.gametype = gametype

    def add_result(self, role, goal):
        self.results[role] = goal
        print(f"new result for {role}: {goal}")


class Simulator(object):
    """Used to make inferences with the provided set of rules."""

    def __init__(self, gdl_rules):
        self.gdl_rules = gdl_rules
        self.engine = ProblogEngine(gdl_rules)

        self._goalnorm = self._get_goalnorms()  # dict<Role, NormFunction> used to normalize goal values

    def _get_goalnorms(self):
        goalnorms = dict()
        for role in self.roles():
            [[mn, mx]] = self.engine.query(query=Term('minmax_goals', *[role, Var('Min'), Var('Max')]),
                                           backend='swipl')
            goalnorms[role] = lambda goal: (goal - int(mn)) / (int(mx) - int(mn))
        return goalnorms

    def get_gametype(self):
        if self.engine.query(query=Term('clause', *[Term('sees', *[None, None]), None]), return_bool=True):
            return GameType.GDL_II
        return GameType.GDL

    # GDL
    def roles(self):
        return self.engine.query(query=Term('role', None))

    def player_roles(self):
        return list(filter(lambda t: t != Term('random'), self.roles()))

    def initial_state(self):
        facts = self.engine.query(query=Term('init', None))
        return State(facts)

    def legal_actions(self, state, role):
        if not self.terminal(
                state):  # legality of action does not depend on terminality of state in most GDL desriptions
            return self.engine.query(query=Term('legal', *[role, None]),
                                     state=state)
        return []

    def legal_jointaction_permutations(self, state):
        legal_actions = dict()
        for role in self.roles():
            legal_actions[role] = self.legal_actions(state, role)
        permutations = [list(action) for action in product(*legal_actions.values())]
        return [JointAction(list(legal_actions.keys()), perm) for perm in permutations]

    def legal_jointaction(self, state):
        actions = self.engine.query(query=Term('legal_jointaction', Var('Action')), state=state, backend='swipl')
        return JointAction(self.roles(), actions)

    def terminal(self, state):
        return self.engine.query(query=Term('terminal'), state=state, return_bool=True)

    def goal(self, state, role, norm=True):
        goal = self.engine.query(Term('goal', *[role, None]), state=state)
        return self._goalnorm[role](int(goal[0])) if norm else int(goal[0])

    def next_state(self, state, jointactions):
        state_actions = state.with_actions(jointactions)
        new_facts = self.engine.query(Term('next', None), state=state_actions)
        return State(new_facts)

    def simulate(self, state, role, rounds=1, norm=True):
        total_goal = 0
        for _ in range(rounds):
            round_goal = self.engine.query(query=Term('simulate', *[list2term(state.facts), role, Var('Value')]),
                                           backend='swipl')
            total_goal += self._goalnorm[role](int(round_goal[0])) if norm else int(round_goal[0])
        return total_goal

    # GDL-II
    def random(self):
        if self.roles().__contains__(Term('random')):
            return {'Role': Term('random'), 'Action': None}
        return None

    def percepts(self, state, role, jointactions):
        state_actions = state.with_actions(jointactions)
        return self.engine.query(query=Term('sees', *[role, None]), state=state_actions)


class State:
    """Represents the state of a game."""
    def __init__(self, facts):
        self.facts = facts

    def with_actions(self, jointactions):
        new_facts = deepcopy(self.facts)
        for role, action in jointactions.actions.items():
            new_facts.append(Term('does', *[role, action]))
        return State(new_facts)

    def with_percepts(self, role, percepts):
        new_facts = deepcopy(self.facts)
        for fact in percepts.facts:
            new_facts.append(Term('sees', *[role, fact]))
        return State(new_facts)

    def sorted(self):
        return sorted(self.facts, key=lambda t: str(t))

    def __repr__(self):
        return ', '.join([str(fact) for fact in self.facts])

    def __eq__(self, other):
        return self.sorted() == other.sorted()


class JointAction:
    """Represents a move for each role in the game."""

    def __init__(self, roles=None, actions=None):
        if roles is not None and actions is not None:
            assert (len(roles) == len(actions))
            self.actions = dict(zip(roles, actions))
        else:
            self.actions = dict()

    def set_move(self, role, action):
        if action is not None:
            self.actions[role] = action

    def get_actions(self):
        return list(self.actions.values())

    def __eq__(self, other):
        return self.actions == other.actions

    def __hash__(self):
        return hash(frozenset(self.actions))

    def __repr__(self):
        return str(self.actions)

    def __len__(self):
        return len(self.actions)


class Percepts:
    """Represents the percepts of a player"""

    def __init__(self, facts):
        self.facts = facts