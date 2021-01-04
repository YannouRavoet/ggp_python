from typing import List
from utils.match_info import GameSettings
from utils.ggp.action import Action
from utils.ggp.jointaction import JointAction
from utils.ggp.percept import Percept
from utils.ggp.percepts import Percepts
from utils.ggp.state import State

from utils.prolog.prolog import PrologEngine


class Simulator(object):
    """Used to make inferences with the provided set of rules. The main functionality of a Simulator is to
        1. translate Prolog query-calls to Python function calls
        2. parse the query results into States, Action, JointAction, Percepts objects.
        Whenever multiple calls to the engine have to be made (e.g. simulations,...), it is best to program this
        functionality in Prolog code to limit the amount of engine calls.
     """

    def __init__(self, gdl_rules):
        self.gdl_rules = gdl_rules
        self.engine = PrologEngine(gdl_rules)
        self._goalnorm = self._create_goalnorms()  # dict<Role, NormFunction> used to normalize goal values

    def _create_goalnorms(self):
        def goalnorm(mn, mx):
            def norm(goal):
                return (goal - mn) / (mx - mn)

            return norm

        goalnorms = dict()
        for role in self.player_roles():
            results = self.engine.query(f"minmax_goals({role}, Min, Max).")
            goalnorms[role] = goalnorm(results[0]['Min'], results[0]['Max'])
        return goalnorms

    def get_gamesettings(self):
        has_random = bool(self.engine.query(query='has_random.'))
        has_imperfect_information = bool(self.engine.query(query='has_imperfect_information.'))
        has_stochastic_actions = bool(self.engine.query(query='has_stochastic_actions.'))
        return GameSettings(has_random, has_imperfect_information, has_stochastic_actions)

    def actions_2_jointaction(self, actions):
        """
        Turns list of actions into a list of JointActions by assigning a role to each action.
        The roles are assigned in the order of self.player_roles().
        :param actions: List of actions to assign roles to
        :return: List of JointActions
        """
        if len(actions) == 0:
            return JointAction([])
        roles = self.player_roles()
        return JointAction([Action(role, action) for role, action in zip(roles, actions)])

    def clear_engine(self):
        result = bool(self.engine.query(query="clear_engine"))
        return result

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    GDL
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    def roles(self) -> List[str]:
        results = self.engine.query(query='roles_pl(R).')
        return PrologEngine.results2string(results[0]['R'])

    def player_roles(self) -> List[str]:
        return list(filter(lambda r: r != 'random', self.roles()))

    def initial_state(self) -> State:
        results = self.engine.query(query='init_pl(State).')
        facts = PrologEngine.results2string(results[0]['State'])
        return State(facts)

    def legal_actions(self, state, role) -> List[Action]:
        if not self.is_terminal(state):
            results = self.engine.query(f'legals_pl({state.to_term()}, {role}, Actions).')
            actions = PrologEngine.results2string(results[0]['Actions'])
            return [Action.from_string(action) for action in actions]
        return []  # legality of action does not depend on terminality of state in most GDL desriptions

    def legal_jointactions(self, state) -> List[JointAction]:
        if not self.is_terminal(state):
            results = self.engine.query(query=f"legal_jointaction({state.to_term()}, JA).")
            return [JointAction([Action.from_string(action) for action in PrologEngine.results2string(result['JA'])])
                    for result in results]
        return []

    def is_terminal(self, state) -> bool:
        return bool(self.engine.query(f"terminal_pl({state.to_term()})."))

    def goal(self, state, role, norm=True) -> float:
        results = self.engine.query(f"goal_pl({state.to_term()}, {role}, Value).")
        return self._goalnorm[role](results[0]['Value']) if norm else results[0]['Value']

    def next_state(self, state, jointaction) -> State:
        results = self.engine.query(f"next_pl({state.to_term()}, {jointaction.to_term()}, NextState).")
        facts = PrologEngine.results2string(results[0]['NextState'])
        return State(facts)

    def simulate(self, state, role, rounds=1, norm=True) -> float:
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
        results = self.engine.query(f"simulate({state.to_term()}, {role}, Value, {rounds}).")
        return self._goalnorm[role](float(results[0]['Value'])) if norm else float(results[0]['Value'])

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    GDL-II
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    def get_percepts(self, state, jointaction, role) -> Percepts:
        results = self.engine.query(query=f"sees_pl({state.to_term()}, {jointaction.to_term()}, {role}, Percepts).")
        return Percepts([Percept.from_string(string) for string in results[0]['Percepts']])

    def update_states_ii(self, states, action, percepts, filter_terminal=False) -> List[State]:
        results = self.engine.query(query=f"update_valid_states("
                                          f"[{','.join([state.to_term() for state in states])}],"
                                          f"{action.to_term()},"
                                          f"{percepts.to_term()},"
                                          f"NewStates,"
                                          f"{'true' if filter_terminal else 'false'}).")
        return [State(facts) for facts in results[0]['NewStates']]

    # Creates a set of all states that are hypothetically true given the action and percept hist of the player's role.
    def create_valid_states_ii(self, action_hist, percept_hist, filter_terminal=False) -> List[State]:
        results = self.engine.query(query=f"create_valid_states("
                                          f"[{','.join([action.to_term() for action in action_hist])}], "
                                          f"[{','.join([percepts.to_term() for percepts in percept_hist])}], "
                                          f"StatesOut, "
                                          f"{'true' if filter_terminal else 'false'}).")
        return [State(facts) for facts in results[0]['StatesOut']]

    def avg_goal(self, states, role, norm=True) -> float:
        results = self.engine.query(query=f"total_goal("
                                          f"[{','.join([state.to_term() for state in states])}], "
                                          f"{role}, "
                                          f"Goal).")
        return self._goalnorm[role](results[0]['Goal'] / len(states)) if norm else results[0]['Goal'] / len(states)

    def filter_states_percepts_ii(self, states, jointactions, role, percepts) -> List[int]:
        results = self.engine.query(query=f"filter_states_percepts("
                                          f"[{','.join([state.to_term() for state in states])}],"
                                          f"[{','.join([ja.to_term() for ja in jointactions])}],"
                                          f"{role},"
                                          f"{percepts.to_term()},"
                                          f"Indices).")
        if not isinstance(results, list):
            return []
        return results[0]['Indices']

    def avg_goal_from_hist(self, action_hist, percept_hist, role, norm=True) -> float:
        results = self.engine.query(query=f"avg_goal_from_history("
                                          f"[{','.join([action.to_term() for action in action_hist])}],"
                                          f"[{','.join([percepts.to_term() for percepts in percept_hist])}],"
                                          f"{role},"
                                          f"AvgGoal).")
        return self._goalnorm[role](results[0]['AvgGoal']) if norm else results[0]['AvgGoal']

    def simulate_states(self, states, role, rounds, norm=True) -> List[float]:
        """ simulate_multi runs rounds Rounds of simulation on each of the provided states and returns a list of
        values for each of the provided states. """
        results = self.engine.query(query=f"simulate_multi("
                                          f"[{','.join([state.to_term() for state in states])}],"
                                          f"{role},"
                                          f"{rounds},"
                                          f"Values).")
        return [self._goalnorm[role](float(result)) if norm else float(result) for result in results[0]['Values']]
