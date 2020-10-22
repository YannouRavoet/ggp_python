from problog.program import PrologString
from problog.logic import Term, Constant
from utils.problog import ProblogEngine
from utils.gdl_parsing import parse_rules_to_string, term_list_to_string
from itertools import product
import copy

class Game(object):
    def __init__(self, gdl_rules):
        self.gdl_rules = gdl_rules
        self.problog_rules = parse_rules_to_string(gdl_rules)
        self.engine = ProblogEngine(self.problog_rules)

        self.turn = 0
        self.roles = self.get_roles()
        self.state = State(self.problog_rules, term_list_to_string(self.get_init()))

    def get_base(self):
        return self.engine.query(Term('base', None))

    def get_init(self):
        return self.engine.query(Term('init', None))

    def get_roles(self):
        return self.engine.query(Term('role', None))

    def get_actions(self, role):
        return self.engine.query(Term('input', *[role, None]))

    def extend_state_with_facts(self, facts):
        self.state.add_facts(facts)

    def calc_next_state(self):
        self.state = State(self.problog_rules, term_list_to_string(self.state.get_next()))
        self.turn += 1

    def get_otherrole_actioncombinations(self, ownrole, ownaction, percepts):
        """
        Given an own role, return a combination of actions that is valid for the percepts and current state of the game
        :param percepts:
        :return:
        """
        legal_actions_all_roles = dict({ownrole: [ownaction]})
        for role in self.roles:
            if role != ownrole:
                legal_actions_all_roles[role] = [make_move_term(role, move) for move in self.state.get_legal_actions(role)]
        action_permutations = [list(a) for a in product(*legal_actions_all_roles.values())]
        valid_permutations = list()
        for perm in action_permutations:
            facts = self.state.problog_state + '\n' + term_list_to_string(perm)
            tmpstate = State(self.state.problog_rules, facts)
            invalid = False
            for percept in percepts:
                if not tmpstate.engine.query_bool(Term('sees', *[ownrole, percept])):
                    invalid = True
                    break
            if not invalid:
                valid_permutations.append(perm)
        return valid_permutations


class State(object):
    def __init__(self, problog_rules, problog_facts):
        """
        A state is represented by a set of the true facts that determine the legality of moves
        and the terminality and goal utility of states.
        :param problog_rules: The set of rules containing at the minimum legal/2, next/1, terminal/0, goal/2.
        :param problog_facts: The set of facts that are true in this state.
        """
        self.problog_rules = problog_rules
        self.problog_state = problog_facts
        self.engine = ProblogEngine(f'{self.problog_rules}\n{self.problog_state}')


    def get_legal_actions(self, role):
        results = self.engine.query(Term('legal', *[role, None]))
        return results

    def get_percepts(self, role):
        """
        Returns the percepts of a role as declared by the sees/2 clause.
        It is assumed apply_moves has been called (aka the necessary does/2 facts have been added to the program).
        :param role: Term representing the role of interest
        :return: list of Terms representing the percepts of the role.
        """
        percepts = self.engine.query(Term('sees', *[role, None]))
        return percepts

    def add_facts(self, facts):
        self.engine.extend(PrologString(term_list_to_string(facts)))
        self.problog_state += '\n' + term_list_to_string(facts)

    def get_next(self):
        """
        Returns the facts of the next state as declared by the next/1 clause.
        It is assumed apply_moves has been called (aka the necessary does/2 facts have been added to the program).
        :return: list of Terms representing the facts of the next state.
        """
        return self.engine.query(Term('next', None))

    def is_terminal(self):
        """
        :return: Whether the state is terminal or not.
        :type return: bool
        """
        return self.engine.query_bool(Term('terminal'))

    def get_goal(self, role):
        return self.engine.query(Term('goal', *[role, None]))

    def __repr__(self):
        return self.problog_state


class Role(Term):
    def __init__(self, role):
        super().__init__(functor='role', *[Constant(role)])
        self.role = self.args[0]


class Action(Term):
    def __init__(self, role, action):
        super().__init__(functor='does', *[self.role, self.action])
        self.role = self.args[0]          # Term representing the role
        self.action = self.args[1]        # Term representing the action


def make_move_term(role, move):
    return Term('does', *[role, move])
