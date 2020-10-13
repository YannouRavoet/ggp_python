from problog.program import PrologString
from problog.logic import Term
from utils.problog import ProblogEngine
from utils.gdl_parsing import parse_rules_to_string, term_list_to_string

class Game(object):
    def __init__(self, gdl_rules):
        self.gdl_rules = gdl_rules
        self.problog_rules = parse_rules_to_string(gdl_rules)
        self.engine = ProblogEngine(self.problog_rules)

        self.roles = self.get_roles()
        self.state = self.get_init()

    def get_base(self):
        # List of base propositions of the game. Every state is defined as a subset of this list.
        return self.engine.query(Term('base', None))

    def get_init(self):
        return State(self, self.engine.query(Term('init', None)))

    def get_roles(self):
        return self.engine.query(Term('role', None))

    def get_actions(self, role):
        # List of all possible actions of a role. Legal actions are defined as a subset of this list.
        return self.engine.query(Term('input', *[role, None]))

    def apply_moves(self, moves):
        self.state = self.state.apply_moves(moves)


class State(object):
    def __init__(self, game, facts):
        """
        A state is represented by a subset of the base facts.
        That subset of facts is true in the state and determines the possible moves and terminality of the state.
        :param problog_rules: The problog rules that determine legality of actions, terminality of states and goal utility.
        :param facts: The subset of facts that is true in this state
        """
        self.game = game
        self.problog_state = PrologString(term_list_to_string(facts))
        self.engine = ProblogEngine(self.game.problog_rules)
        self.engine.extend(self.problog_state)

    def get_legal_actions(self, role):
        return self.engine.query(Term('legal', *[role, None]))

    def apply_moves(self, moves):
        """
        Applies the provided moves to the current state and returns a State object representing the resulting state.
        :param moves: A List of moves of size len(roles)
        :return: State representing the resulting state
        """
        roles = self.game.roles
        assert (len(moves) == len(roles))
        # TODO: check whether moves are legal, and replace with legal moves if necessary

        # 1. Extend engine.db with actions
        action_terms = []
        for i in range(len(moves)):
            action_terms.append('does(' + str(roles[i]) + ',' + str(moves[i]) + ')')
        self.engine.extend(PrologString(term_list_to_string(action_terms)))

        # 2. Inference next state
        next_facts = self.engine.query(Term('next', None))
        return State(self.game, next_facts)

    def is_terminal(self):
        """
        :return: Whether the state is terminal
        """
        return self.engine.query(Term('terminal'))

    def get_goal(self, role):
        return self.engine.query(Term('goal', *[role, None]))
