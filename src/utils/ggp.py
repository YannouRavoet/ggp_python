from copy import deepcopy
from utils.match_info import GameType
from utils.problog import ProblogEngine
from problog.logic import Term, Var, list2term, term2list


class Simulator(object):
    """Used to make inferences with the provided set of rules."""

    def __init__(self, gdl_rules):
        self.gdl_rules = gdl_rules
        self.engine = ProblogEngine(gdl_rules)

        self._goalnorm = self._get_goalnorms()  # dict<Role, NormFunction> used to normalize goal values

    def _get_goalnorms(self):
        def goalnorm(mn, mx):
            def norm(goal):
                return (goal - mn) / (mx - mn)
            return norm

        goalnorms = dict()
        for role in self.roles():
            [[mn, mx]] = self.engine.query(query=Term('minmax_goals', *[role, Var('Min'), Var('Max')]),
                                           backend='swipl')
            goalnorms[role] = goalnorm(int(mn), int(mx))
        return goalnorms

    def get_gametype(self):
        if self.engine.query(query=Term('clause', *[Term('sees', *[None, None]), None]), return_bool=True):
            return GameType.GDL_II
        return GameType.GDL

    """""""""""
        GDL
    """""""""""
    def roles(self):
        return self.engine.query(query=Term('role', None))

    def player_roles(self):
        return list(filter(lambda t: t != Term('random'), self.roles()))

    def initial_state(self):
        facts = self.engine.query(query=Term('init', None))
        return State(facts)

    def legal_actions(self, state, role):
        if not self.terminal(state):
            return self.engine.query(query=Term('legal', *[role, None]),
                                     state=state)
        return []  # legality of action does not depend on terminality of state in most GDL desriptions

    def legal_jointaction_permutations(self, state):
        [jointactions] = self.engine.query(query=Term('legal_jointaction_perm', Var('JointActions')),
                                           state=state,
                                           backend='swipl')
        return [JointAction.from_term(term) for term in term2list(jointactions)]

    def terminal(self, state):
        return self.engine.query(query=Term('terminal'), state=state, return_bool=True)

    def goal(self, state, role, norm=True):
        [goal] = self.engine.query(Term('goal', *[role, None]), state=state)
        return self._goalnorm[role](int(goal)) if norm else int(goal)

    def next_state(self, state, jointactions):
        state_actions = state.with_jointaction(jointactions)
        new_facts = self.engine.query(Term('next', None), state=state_actions)
        return State(new_facts)

    def simulate(self, state, role, rounds=1, norm=True):
        total_goal = 0
        for _ in range(rounds):
            round_goal = self.engine.query(query=Term('simulate', *[list2term(state.facts), role, Var('Value')]),
                                           backend='swipl')
            if len(round_goal):
                total_goal += self._goalnorm[role](int(round_goal[0])) if norm else int(round_goal[0])
            else:   # HAPPENS WHEN SIMULATION IS STOPPED DUE TO TIMEOUT
                return 0
        return total_goal

    """""""""""
       GDL-II
    """""""""""
    def random(self):
        class Environment:
            def __init__(self):
                self.role = Term('random')
                self.action = None
        if self.roles().__contains__(Term('random')):
            return Environment()
        return None

    def percepts(self, state, role, jointactions):
        state_actions = state.with_jointaction(jointactions)
        terms = self.engine.query(query=Term('sees', *[role, None]), state=state_actions)
        return terms

    # TODO
    def valid_state(self, state, role, percepts):
        return True

    # TODO
    def create_valid_state(self, role, action_hist, percept_hist):
        return State()

    def legal_jointaction_from_percepts(self, state, role, role_action, role_percepts):
        state_action = state.with_jointaction(JointAction([role], [role_action]))
        [jointaction] = self.engine.query(query=Term('legal_jointaction_from_percepts',
                                                   *[role, role_percepts, Var('JointAction')]),
                                        state=state_action, backend='swipl')
        jointaction = JointAction.from_term(term2list(jointaction))
        jointaction.set_move(role, role_action)
        return jointaction


class State:
    """Represents the state of a game."""
    def __init__(self, facts):
        self.facts = facts

    def with_jointaction(self, jointaction):
        new_facts = deepcopy(self.facts)
        return State(new_facts + jointaction.to_terms())

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

    @classmethod
    def from_term(cls, terms):
        roles = [term.args[0] for term in terms]
        actions = [term.args[1] for term in terms]
        return cls(roles, actions)

    def to_terms(self):
        return [Term('does', *[role, action]) for role, action in self.actions.items()]

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
        return str(list(self.actions.values()))

    def __len__(self):
        return len(self.actions)


class Percepts:
    """Represents the percepts of a player"""
    def __init__(self, role, facts):
        self.role = role
        self.percepts = facts

    def to_term(self):
        terms = [Term('sees', *[self.role, fact]) for fact in self.percepts]
        return list2term(terms)

    def __repr__(self):
        return str(list(self.percepts))
