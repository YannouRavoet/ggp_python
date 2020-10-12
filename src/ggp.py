from utils.gdl_parsing import parse_rules_string, term_list_to_string
from utils.problog import ProblogEngine
from problog.logic import Term
from problog.program import PrologString

class GameManager(object):
    def __init__(self):
        self.games = dict()

    def add_game(self, gameID, gdl_rules, startclock, playclock):
        self.games[gameID] = Game(gameID, gdl_rules, startclock, playclock)
        return self.games[gameID]

    def get_game(self, gameID):
        return self.games[gameID]

    def add_player(self, gameID):
        game = self.get_game(gameID)



class Game(object):
    def __init__(self, gameID, gdl_rules, startclock, playclock):
        self.gameID = gameID
        self.gdl_rules = gdl_rules
        self.problog_rules = parse_rules_string(gdl_rules)
        self.engine = ProblogEngine(self.problog_rules)
        self.startclock = startclock
        self.playclock = playclock
        self.players = dict()

    def base(self):
        # List of base propositions of the game. Every state is defined as a subset of this list.
        return self.engine.query(Term('base', None))

    def init(self):
        return State(self.problog_rules, self.engine.query(Term('init', None)))

    def roles(self):
        return self.engine.query(Term('role', None))

    def actions(self, role):
        # List of all possible actions of a role. Legal actions are defined as a subset of this list.
        return self.engine.query(Term('input', *[Term(role), None]))

class State(object):
    def __init__(self, problog_rules, facts):
        """
        A state is represented by a subset of the base facts.
        That subset of facts is true in the state and determines the possible moves and terminality of the state.
        :param problog_rules: The problog rules that determine legality of actions, terminality of states and goal utility.
        :param facts: The subset of facts that is true in this state
        """
        self.problog_rules = PrologString(problog_rules)
        self.problog_state = PrologString(term_list_to_string(facts))
        self.engine = ProblogEngine(problog_rules)
        self.engine.extend(self.problog_state)

    def legal_actions(self, role):
        return self.engine.query(Term('legal', *[Term(role), None]))



class Player(object):
    def __int__(self, name, gdl_rules, role):
        self.name = name
        self.role = role



    # (START MATCHID ROLE GAMEDESCRIPTION STARTCLOCK PLAYCLOCK)
    def cmd_start(self):
        pass

    # (PLAY MATCHID JOINTMOVE)
    def cmd_play(self):
        pass

    # (STOP MATCHID JOINTMOVE)
    def cmd_stop(self):
        pass

