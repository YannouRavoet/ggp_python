from utils.ggp.action import Action
from utils.ggp.jointaction import JointAction
from utils.ggp.percepts import Percepts
from utils.ggp.state import State
from utils.match_info import GameType
from utils.problog.problog import ProblogEngine
from problog.logic import Term, Var, list2term, term2list, Constant


class Simulator(object):
    """Used to make inferences with the provided set of rules. The main functionality of a Simulator is to
        1. translate Prolog and Problog query-calls to Python function calls
        2. parse the query results into States, Action, JointAction, Percepts objects.
        Whenever multiple calls to the engine have to be made (e.g. simulations,...), it is best to program this
        functionality in Prolog/ProbLog code to limit the amount of engine calls.
     """

    def __init__(self, gdl_rules):
        self.gdl_rules = gdl_rules
        self.engine = ProblogEngine(gdl_rules)
        self._goalnorm = self._create_goalnorms()  # dict<Role, NormFunction> used to normalize goal values

    def _create_goalnorms(self):
        def goalnorm(mini, maxi):
            def norm(goal):
                return (goal - mini) / (maxi - mini)

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

    def actions_2_jointaction(self, actions):
        """
        Turns list of actions into a list of JointActions by assigning a role to each action.
        The roles are assigned in the order of self.player_roles().
        :param actions: List of actions to assign roles to
        :return: List of JointActions
        """
        roles = self.player_roles()
        assert (len(actions) == len(roles))
        return JointAction([Action(role, action) for role, action in zip(roles, actions)])

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    GDL
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

    def legal_jointactions(self, state):
        jointactions = self.engine.query(query=Term('legal_jointaction', *[state.to_term(), Var('JointActions')]),
                                         backend='swipl')
        return [JointAction().from_term(jointaction) for jointaction in jointactions]

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
        """
        Simulates the game from the given state on, selecting a random JointAction at each consecutive state.
        When a terminal state is reached, the goal value of 'role' is returned. If 'rounds' > 1, it sums up the goal
        values and returns this sum.
        :param state: Starting State of the simulation
        :param role: Role to calculate goal values from
        :param rounds: Number of simulation rounds to run
        :param norm: Whether or not to normalize the goal values
        :return: Sum of goal values across all simulation rounds
        """
        [goal] = self.engine.query(query=Term('simulate', *[state.to_term(), role, Var('Value'), Constant(rounds)]),
                                   backend='swipl')
        return self._goalnorm[role](int(goal)) if norm else int(goal)

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    GDL-II
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    def random(self):
        class Environment:
            def __init__(self):
                self.role = Term('random')
                self.action = None

        if self.roles().__contains__(Term('random')):
            return Environment()
        return None

    def percepts(self, state, jointaction, role):
        [term] = self.engine.query(
            query=Term('sees_pl', *[state.to_term(), jointaction.to_term(), role, Var('Percepts')]),
            backend="swipl")
        return Percepts.from_term(term)

    def update_states_ii(self, states, action, percepts, filter_terminal=False):
        [states] = self.engine.query(query=Term('update_valid_states', *[list2term([s.to_term() for s in states]),
                                                                         action.to_term(),
                                                                         percepts.to_term(),
                                                                         Var('NewStates'),
                                                                         Term('true' if filter_terminal else 'false')]),
                                     backend="swipl")
        return [State(facts) for facts in term2list(states)]

    # Creates a set of all states that are hypothetically true given the action and percept hist of the player's role.
    def create_valid_states_ii(self, action_hist, percept_hist, filter_terminal=False):
        [states] = self.engine.query(query=Term('create_valid_states', *[list2term([a.to_term() for a in action_hist]),
                                                                         list2term([s.to_term() for s in percept_hist]),
                                                                         Var('StatesOut'),
                                                                         Term('true' if filter_terminal else 'false')]),
                                     backend="swipl")
        return [State(facts) for facts in term2list(states)]

    def avg_goal(self, states, role, norm=True):
        [total_goal] = self.engine.query(query=Term('total_goal', *[list2term([s.to_term() for s in states]),
                                                                    role,
                                                                    Var('Goal')]),
                                         backend="swipl")

        return self._goalnorm[role](int(total_goal)/len(states)) if norm else int(total_goal)/len(states)
