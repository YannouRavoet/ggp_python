import random
from copy import deepcopy
from orderedset import OrderedSet
from utils.match_info import GameType
from utils.problog import ProblogEngine
from problog.logic import Term, Var, list2term, term2list, Constant


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
        for role in self.player_roles():
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
        [roles_term] = self.engine.query(query=Term('roles_pl', Var('Roles')),
                                         backend="swipl")
        return term2list(roles_term)

    def player_roles(self):
        return list(filter(lambda t: t != Term('random'), self.roles()))

    def initial_state(self):
        [facts] = self.engine.query(query=Term('init_pl', Var('State')),
                                    backend="swipl")
        return State.from_term(facts)

    def legal_actions(self, state, role):
        if not self.terminal(state):
            results = self.engine.query(query=Term('legal_pl', *[state.to_term(), role, Var('Actions')]),
                                        backend="swipl")
            return [Action.from_term(action_term) for action_term in results]
        return []  # legality of action does not depend on terminality of state in most GDL desriptions

    def legal_jointaction_permutations(self, state):
        [jointactions] = self.engine.query(
            query=Term('legal_jointaction_perm', *[state.to_term(), Var('JointActions')]),
            backend='swipl')
        return [JointAction([Action.from_term(action) for action in action_list]) for action_list in
                term2list(jointactions)]

    def terminal(self, state):
        return self.engine.query(query=Term('terminal_pl', state.to_term()),
                                 backend="swipl", return_bool=True)

    def goal(self, state, role, norm=True):
        [goal] = self.engine.query(Term('goal_pl', *[state.to_term(), role, Var('Value')]),
                                   backend="swipl")
        return self._goalnorm[role](int(goal)) if norm else int(goal)

    def next_state(self, state, jointaction):
        [facts] = self.engine.query(Term('next_pl', *[state.to_term(), jointaction.to_term(), Var('NextState')]),
                                    backend="swipl")
        return State.from_term(facts)

    def simulate(self, state, role, rounds=1, norm=True):
        goal = self.engine.query(query=Term('simulate', *[state.to_term(), role, Var('Value'), Constant(rounds)]),
                                 backend='swipl')
        # TODO: Find out why this sometimes happens
        if len(goal) > 0:
            return self._goalnorm[role](int(goal[0])) if norm else int(goal[0])
        print("simulation failed")
        return 0

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

    def percepts(self, state, jointaction, role):
        [term] = self.engine.query(query=Term('sees_pl', *[state.to_term(), jointaction.to_term(), role, Var('Percepts')]),
                                   backend="swipl")
        if term != Term('[]'):
            return Percepts.from_term(term)
        return Percepts(role, list())

    def valid_state(self, state, percepts):
        result = self.engine.query(query=Term('valid_state', *[state.to_term(), percepts.to_term()]),
                                   backend="swipl", return_bool=True)
        return result

    def create_valid_state(self, role, action_hist, percept_hist):
        state_terms = self.engine.query(query=Term('create_valid_state', *[role,
                                                                           list2term([a.to_term() for a in action_hist]),
                                                                           list2term([p.to_term() for p in percept_hist]),
                                                                           Var('State')]),
                                        backend="swipl")
        return State.from_term(random.choice(state_terms))

    def legal_jointaction_from_percepts(self, state, action, percepts):
        jointactions = self.engine.query(query=Term('legal_jointaction_from_percepts',
                                                    *[state.to_term(), action.to_term(), percepts.to_term(), Var('JointAction')]),
                                         backend='swipl')
        return JointAction.from_term(random.choice(jointactions))


class State:
    """Represents the state of a game."""

    def __init__(self, facts):
        self.facts = facts

    def with_actions(self, actions):
        new_facts = deepcopy(self.facts)
        return State(new_facts + [action.to_term() for action in actions])

    def with_jointaction(self, jointaction):
        new_facts = deepcopy(self.facts)
        return State(new_facts + [action.to_term() for action in jointaction.actions])

    def sort(self):
        self.facts = sorted(self.facts, key=lambda t: str(t))
        return self

    def __repr__(self):
        return ', '.join([str(fact) for fact in self.facts])

    @classmethod
    def from_term(cls, term):
        return cls(term2list(term))

    def to_term(self):
        return list2term(self.facts)


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


class JointAction:
    """Represents a move for each role in the game."""

    def __init__(self, actions=None):
        self.actions = OrderedSet(actions) if actions is not None else OrderedSet()

    @classmethod
    def from_term(cls, term):
        actions = [Action.from_term(action_term) for action_term in term2list(term)]
        return cls(actions)

    def to_term(self):
        return list2term([action.to_term() for action in self.actions])

    def add_action(self, action):
        if action is not None:
            self.actions.add(action)

    def get_action(self, role):
        for action in self.actions:
            if action.role == role:
                return action
        return None

    def __repr__(self):
        return str([action.action for action in self.actions])

    def __len__(self):
        return len(self.actions)

    def __eq__(self, other):
        return self.actions == other.actions

    def __hash__(self):
        return hash(frozenset(self.actions))


class Percepts:
    """Represents the percepts of a player"""

    def __init__(self, role, percepts):
        self.role = role
        self.percepts = percepts

    @classmethod
    def from_term(cls, term):
        role = term2list(term)[0].args[0]
        percepts = [sees_term.args[1] for sees_term in term2list(term)]
        return cls(role, percepts)

    def to_term(self):
        terms = [Term('sees', *[self.role, percept]) for percept in self.percepts]
        return list2term(terms)

    def __repr__(self):
        return str(self.percepts)
