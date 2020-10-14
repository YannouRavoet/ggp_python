from problog.program import PrologString
from problog.logic import Term
from utils.problog import ProblogEngine
from utils.gdl_parsing import parse_rules_to_string, term_list_to_string
import random


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

    def extend_state_with_moves(self, moves):
        for i in range(len(moves)):
            is_legal = self.engine.query(Term('legal',*[moves[i].args[0], moves[i].args[1]]))
            if not is_legal:
                moves[i] = random.choice(self.engine.query(Term('legal', *[moves[i].args[0], None])))
        self.state.add_facts(moves)

    def extend_state_with_facts(self, facts):
        self.state.add_facts(facts)

    def calc_next_state(self):
        self.state = State(self.problog_rules, term_list_to_string(self.state.get_next()))
        self.turn += 1


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
        return self.engine.query(Term('legal', *[role, None]))

    def get_percepts(self, role):
        """
        Returns the percepts of a role as declared by the sees/2 clause.
        It is assumed apply_moves has been called (aka the necessary does/2 facts have been added to the program).
        :param role: Term representing the role of interest
        :return: list of Terms representing the percepts of the role.
        """
        return self.engine.query(Term('sees', *[role, None]))

    def add_facts(self, facts):
        self.engine.extend(PrologString(term_list_to_string(facts)))

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
        return self.engine.query(Term('terminal'))

    def get_goal(self, role):
        return self.engine.query(Term('goal', *[role, None]))


def make_move_term(role, move):
    return Term('does', *[role, move])
