from typing import List, Dict
from utils.match_info import GameSettings
from utils.ggp.action import Action
from utils.ggp.jointaction import JointAction
from utils.ggp.percept import Percept
from utils.ggp.percepts import Percepts
from utils.ggp.state import State
from utils.prolog.prolog import PrologEngine


class I_InferenceInterface:
    """Used to make inferences with the provided set of rules. The main functionality of a Simulator is to
        1. translate Prolog query-calls to Python function calls
        2. parse the query results into States, Action, JointAction, Percepts objects.
        Whenever multiple calls to the engine have to be made (e.g. simulations,...), it is best to program this
        functionality in Prolog code to limit the amount of engine calls.
     """

    def get_gamesettings(self) -> GameSettings:
        """
        Finds the GameSettings of loaded game
        :return: gamesettings of the game"""
        pass

    def clear_engine(self):
        """
        clears the engine of any previous asserted information. Useful in case a query was interrupted."""
        pass

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    GDL
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    def get_roles(self) -> List[str]:
        """
        returns a list of all roles as defined by *role/1*.
        WARNING: the list is ordered, so the order might not be the same as the original ruleset.
        :return: list of all roles in the game"""
        pass

    def get_player_roles(self) -> List[str]:
        """
        returns a list of all roles played by GamePlayers.
        WARNING: the list is ordered, so the order might not be the same as the original ruleset.
        :return: list of all roles that must be assumed by players"""
        pass

    def initial_state(self) -> State:
        """
        Finds the initial state as defined by *init/1*.
        :return: initial state of the game"""
        pass

    def legal_actions(self, state: State, role: str) -> List[Action]:
        """
        returns a list of all legal Actions for the GamePlayer with role *role* in State *state* as defined by
        *legal/2*.
        :param state: current state of the game
        :param role: role for which to search legal actions
        :return: list of legal actions that can be performed in the current state"""
        pass

    def actionlist2jointaction(self, actions: List[Action]) -> JointAction:
        """
        Turns list of actions into a list of JointActions by assigning a role to each action.
        The roles are assigned in the order of self.player_roles().
        :param actions: list of actions to assign roles to
        :return: list of jointactions
        """
        pass

    def legal_jointactions(self, state: State) -> List[JointAction]:
        """
        returns a list of legal JointActions in State *state* as defined by *legal/2*.
        :param state: current state of the game
        :return: list of legal jointactions that can be performed in the current state"""
        pass

    def is_terminal(self, state: State) -> bool:
        """
        returns whether or not the State *state* is terminal as defined by *terminal/0*.
        :param state: current state of the game
        :return: whether the state is terminal"""
        pass

    def goal(self, state: State, role: str, norm: bool = True) -> float:
        """
        returns the goal value of the GamePlayer with role *role* in State *state* as defined by *goal/2*.
        If *norm* is True, the goal value is normalized.
        :param state: current state of the game
        :param role: role for which to infer the goal value
        :param norm: whether or not to normalize the goal value
        :return: goal value of player in state"""
        pass

    def next_state(self, state: State, jointaction: JointAction) -> State:
        """
        returns the next State if in State *state* JointAction *jointaction* is asserted as defined by *next/1*.
        :param state: current state of the game
        :param jointaction: jointaction that is take in the current state
        :return: next state of the game"""
        pass

    def simulate(self, state: State, role: str, rounds: int, norm: bool = True) -> float:
        """
        Simulates the game starting from the given State *state*, selecting a random JointAction at every consecutive
        state until a terminal state is reached.
        When a terminal state is reached, the goal value of 'role' is returned.
        If *rounds* > 1, runs multiple rounds and returns the sums of the goal values in all rounds.
        If *norm* is True, all goal values are normalized. The sum is **NOT** normalized.
        :param state: starting state of the simulation
        :param role: role to calculate goal values for
        :param rounds: number rounds of simulation to run
        :param norm: whether or not to normalize the goal values
        :return: sum of goal values across all simulation rounds
        """

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    GDL-II
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    def get_percepts(self, state: State, jointaction: JointAction, role: str) -> Percepts:
        """Finds the percepts of the GamePlayer with role *role* in State *state* if the JointAction *jointaction*
        was taken as defined by *sees/2*.
        :param state: current state of the game
        :param jointaction: jointaction taken in the current state of the game
        :param role: role of the player whose percepts we search
        :return: percepts of the player"""
        pass

    def generate_jointaction_from_percepts(self, state: State, ownaction: Action, percepts: Percepts) -> JointAction:
        """Tries to complete as much as possible of a jointaction in the State *state* that,
        given the Action *ownaction* and Percepts *percepts* of the player calling the method, is valid.
        :param state: state of the game in which to evaluate the percepts
        :param ownaction: action of the player calling the method
        :param percepts: percepts of the player calling the method
        :param role: role of the player calling the method
        :return: a list of facts that have to be true, for the percepts to be valid"""
        pass

    def update_states_ii(self, states: List[State], action: Action, percepts: Percepts,
                         filter_terminal: bool = False) -> List[State]:
        """Creates a list of states that can be successor states to at least one State in *states* given Action
        *action* and Percepts *percepts*.
        If *filter_terminal* is True, only terminal states are kept.
        If *filter_terminal* is False, only non-terminal states are kept.
        :param states: list of potential current states
        :param action: action of the player calling the method
        :param percepts: percepts of the player calling the method
        :param filter_terminal: whether to keep only terminal or non-terminal states
        :return: list of valid successor states"""
        pass

    def create_valid_states_ii(self, action_hist: List[Action], percept_hist: List[Percepts],
                               filter_terminal: bool = False) -> List[State]:
        """Creates a set of all current states that are valid given the Action history *action_hist* and Percepts
        history *percept_hist*.
        If *filter_terminal* is True, only terminal states are kept.
        If *filter_terminal* is False, only non-terminal states are kept.
        :param action_hist: history of the action taken by the player calling the method
        :param percept_hist: history of the percepts received by the player calling the method
        :param filter_terminal: whether to keep only terminal or non-terminal states
        :return: list of valid current states"""
        pass

    def filter_states_percepts_ii(self, states: List[State], jointactions: List[JointAction], role: str,
                                  percepts: Percepts) -> List[int]:
        """filters all states from the list of States *states* that when the corresponding JointAction from the list
        of JointActions *jointactions* is taken, results in the Percepts *percepts* for the role *role*.
        Returns a list of the indices of states that passed the filter.
        WARNING: len(jointactions) must be equal to len(states).
        :param states: list of states to filter
        :param jointactions: list of jointactions corresponding to the states
        :param role: role of the player calling the method
        :param percepts: percepts perceived by the player in the valid state
        :return: list of indices of all valid states in the original list
        """
        pass

    def avg_goal_from_hist(self, action_hist: List[Action], percept_hist: List[Percepts], role: str,
                           norm: bool = True) -> float:
        """Create the complete set of valid terminal states from the given Action history *action_hist* and
        Percepts history *percept_hist* for the GamePlayer with role *role*.
        If *norm* is True, the average goal is normalized.
        :param action_hist: history of actions taken by the calling player
        :param percept_hist: history of percepts received by the calling player
        :param role: role of the calling player
        :param norm: whether or not to normalize the result
        :return: average goal over the complete set of valid terminal states."""
        pass

    def simulate_states(self, states: List[State], role: str, rounds: int, norm: bool = True) -> List[float]:
        """simulates the game starting from each State in *states* by choosing random JointActions in each consecutive
        state, until a terminal state is reached. When a terminal state is reached, the goal value for the player with
        role *role* is calculated.
        Runs *rounds* rounds of simulation for each State in *states*.
        If *norm* is True, the goal values are normalized.
        Returns a list with the sum of the goal values for each state in *states*.
        :param states: list of states the start the simulations from
        :param role: role of the player whose goal value we calculate
        :param rounds: number of rounds of simulation to do for each state in *states*
        :param norm: whether or not to normalize the goal values
        :return: list of the sum of goal values for the simulations for every state in *states*"""
        pass

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    Stochastic GDL
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    def get_action_outcomes(self, state: State, stochastic_action: Action) -> Dict[Action, float]:
        """Returns the outcomes and their corresponding probabilities of taking Action *action* in State *state*
        :param state: current state of the game
        :param action: action for which the outcomes are returned
        :return:list of tuples with the possible outcome and corresponding probability"""
        pass

    def legal_jointaction_sto(self, state: State) -> Dict[JointAction, Dict[JointAction, float]]:
        """Returns all legal jointactions in state *State* and their possible outcomes and probabilities
        :param state: current state of the game
        :return:Dict[legal jointactions, Dict[possible outcome jointactions, corresponding probability]]"""
        pass

    def sample_outcome(self, state: State, action: Action):
        """Samples the outcome of the Action *action*, in State *state* as defined by *outcome/4*.
        Returns the sampled outcome.
        :param state: the current state of the game
        :param action: action to sample the outcome from
        :return: the sampled outcome"""
        pass

    def sample_jointaction_outcome(self, state: State, jointaction: JointAction):
        """Samples the outcome of the JointAction *jointaction*, in State *state* as defined by *outcome/4*.
        Returns the sampled outcome.
        :param state: the current state of the game
        :param jointaction: jointaction to sample the outcome from
        :return: the sampled outcome"""
        pass

    def simulate_sto(self, state: State, role: str, rounds: int, norm: bool=True) -> float:
        """
        Simulates the game starting from the given State *state*, selecting a random JointAction at every consecutive
        state until a terminal state is reached.
        When a terminal state is reached, the goal value of 'role' is returned.
        If *rounds* > 1, runs multiple rounds and returns the sums of the goal values in all rounds.
        If *norm* is True, all goal values are normalized. The sum is **NOT** normalized.
        The only difference with simulate is that we have to sample the random JointAction for outcomes.
        :param state: starting state of the simulation
        :param role: role to calculate goal values for
        :param rounds: number rounds of simulation to run
        :param norm: whether or not to normalize the goal values
        :return: sum of goal values across all simulation rounds
        """
        pass


class InferenceInterface(I_InferenceInterface):
    def __init__(self, gdl_rules):
        self.engine = PrologEngine(gdl_rules)
        self.roles = self.get_roles()
        self._goalnorm = self._create_goalnorms()  # dict<Role, NormFunction> used to normalize goal values

    def _create_goalnorms(self):
        def goalnorm(mn, mx):
            def norm(goal):
                return (goal - mn) / (mx - mn)

            return norm

        goalnorms = dict()
        for role in self.get_player_roles():
            results = self.engine.query(f"minmax_goals({role}, Min, Max)")
            goalnorms[role] = goalnorm(results[0]['Min'], results[0]['Max'])
        return goalnorms

    def get_gamesettings(self):
        has_random = bool(self.engine.query(query='has_random'))
        has_imperfect_information = bool(self.engine.query(query='has_imperfect_information'))
        has_stochastic_actions = bool(self.engine.query(query='has_stochastic_actions'))
        return GameSettings(has_random, has_imperfect_information, has_stochastic_actions)

    def clear_engine(self):
        self.engine.query(query="clear_engine")

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    GDL
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    def get_roles(self):
        results = self.engine.query(query='roles_pl(R)')
        return PrologEngine.results2list(results[0]['R'])

    def get_player_roles(self):
        return list(filter(lambda r: r != 'random', self.roles))

    def initial_state(self):
        results = self.engine.query(query='init_pl(State)')
        facts = PrologEngine.results2list(results[0]['State'])
        return State(facts)

    def legal_actions(self, state, role):
        if not self.is_terminal(state):
            results = self.engine.query(f'legals_pl({state.to_term()}, {role}, Actions)')
            actions = PrologEngine.results2list(results[0]['Actions'])
            return [Action.from_string(action) for action in actions]
        return []  # legality of action does not depend on terminality of state in most GDL desriptions

    def actionlist2jointaction(self, actions):
        if len(actions) == 0:
            return JointAction([])
        roles = self.get_player_roles()
        return JointAction([Action(role, action) for role, action in zip(roles, actions)])

    def legal_jointactions(self, state):
        if not self.is_terminal(state):
            results = self.engine.query(query=f"legal_jointaction({state.to_term()}, JA)")
            return [JointAction([Action.from_string(action) for action in PrologEngine.results2list(result['JA'])])
                    for result in results]
        return []

    def is_terminal(self, state):
        return bool(self.engine.query(f"terminal_pl({state.to_term()})"))

    def goal(self, state, role, norm=True):
        results = self.engine.query(f"goal_pl({state.to_term()}, {role}, Value)")
        return self._goalnorm[role](results[0]['Value']) if norm else results[0]['Value']

    def next_state(self, state, jointaction):
        assert(len(jointaction) == len(self.roles))
        results = self.engine.query(f"next_pl({state.to_term()}, {jointaction.to_term()}, NextState)")
        facts = PrologEngine.results2list(results[0]['NextState'])
        return State(facts)

    def simulate(self, state: State, role: str, rounds: int, norm: bool=True):
        results = self.engine.query(f"simulate({state.to_term()}, {role}, Value, {rounds})")
        return self._goalnorm[role](float(results[0]['Value'])) if norm else float(results[0]['Value'])

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    GDL-II
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    def get_percepts(self, state, jointaction, role):
        results = self.engine.query(query=f"sees_pl({state.to_term()}, {jointaction.to_term()}, {role}, Percepts)")
        return Percepts([Percept.from_string(string) for string in results[0]['Percepts']])

    def generate_jointaction_from_percepts(self, state, ownaction, percepts):
        results = self.engine.query(query=f"generate_jointactions("
                                          f"{state.to_term()},"
                                          f"{ownaction.to_term()},"
                                          f"{percepts.to_term()},"
                                          f"JointActions)")
        return [JointAction([Action.from_string(action) for action in jointaction])
                for jointaction in results[0]['JointActions']]

    def update_states_ii(self, states, action, percepts, filter_terminal=False):
        results = self.engine.query(query=f"update_valid_states("
                                          f"[{','.join([state.to_term() for state in states])}],"
                                          f"{action.to_term()},"
                                          f"{percepts.to_term()},"
                                          f"NewStates,"
                                          f"{'true' if filter_terminal else 'false'})")
        return [State(facts) for facts in results[0]['NewStates']]

    def create_valid_states_ii(self, action_hist, percept_hist, filter_terminal=False):
        results = self.engine.query(query=f"create_valid_states("
                                          f"[{','.join([action.to_term() for action in action_hist])}], "
                                          f"[{','.join([percepts.to_term() for percepts in percept_hist])}], "
                                          f"StatesOut, "
                                          f"{'true' if filter_terminal else 'false'})")
        return [State(facts) for facts in results[0]['StatesOut']]

    def filter_states_percepts_ii(self, states, jointactions, role, percepts):
        results = self.engine.query(query=f"filter_states_percepts("
                                          f"[{','.join([state.to_term() for state in states])}],"
                                          f"[{','.join([ja.to_term() for ja in jointactions])}],"
                                          f"{role},"
                                          f"{percepts.to_term()},"
                                          f"Indices)")
        if not isinstance(results, list):
            return []
        return results[0]['Indices']

    def avg_goal_from_hist(self, action_hist, percept_hist, role, norm=True):
        results = self.engine.query(query=f"avg_goal_from_history("
                                          f"[{','.join([action.to_term() for action in action_hist])}],"
                                          f"[{','.join([percepts.to_term() for percepts in percept_hist])}],"
                                          f"{role},"
                                          f"AvgGoal)")
        return self._goalnorm[role](results[0]['AvgGoal']) if norm else results[0]['AvgGoal']

    def simulate_states(self, states, role, rounds, norm=True):
        results = self.engine.query(query=f"simulate_multi("
                                          f"[{','.join([state.to_term() for state in states])}],"
                                          f"{role},"
                                          f"{rounds},"
                                          f"Values)")
        return [self._goalnorm[role](float(result)) if norm else float(result) for result in results[0]['Values']]

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                                                    Stochastic GDL
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    def get_action_outcomes(self, state, stochastic_action):
        results = self.engine.query(query=f"outcome_pl("
                                          f"{state.to_term()}, "
                                          f"{stochastic_action.role}, "
                                          f"{stochastic_action.action},"
                                          f"Outcomes)")
        outcomes = dict()
        for outcome in results[0]['Outcomes']:
            outcomes[Action.from_string(outcome[0])] = outcome[1]
        return outcomes

    def legal_jointaction_sto(self, state):
        """Returns all legal JointActions along with their Deterministic JointAction outcomes
        and the corresponding probability."""
        if not self.is_terminal(state):
            results = self.engine.query(query=f"legal_jointaction_sto("
                                              f"{state.to_term()}, "
                                              f"JointAction,"
                                              f"Outcomes)")
            returndict = dict()
            for result in results:
                jointaction = JointAction([Action.from_string(action) for action in result['JointAction']])
                outcomes = dict()
                for outcome in result['Outcomes']:
                    outcomes[JointAction([Action.from_string(action) for action in outcome[0]])] = outcome[1]
                returndict[jointaction] = outcomes
            return returndict
        return []

    def sample_outcome(self, state, action):
        results = self.engine.query(query=f"sample_action_outcome({state.to_term()}, {action.role}, {action.action}, Outcome)")
        return Action.from_string(results[0]['Outcome'])

    def sample_jointaction_outcome(self, state, jointaction):
        results = self.engine.query(query=f"sample_jointaction_outcome({state.to_term()}, {jointaction.to_term()}, Outcome)")
        return JointAction([Action.from_string(action) for action in results[0]['Outcome']])

    def simulate_sto(self, state, role, rounds, norm=True):
        results = self.engine.query(f"simulate_sto({state.to_term()}, {role}, Value, {rounds})")
        if not len(results):
            print(results)
        return self._goalnorm[role](float(results[0]['Value'])) if norm else float(results[0]['Value'])



