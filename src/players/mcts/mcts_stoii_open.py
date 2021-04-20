import math
import random
from typing import Dict, List
import stopit
from math import sqrt, log
from gameplayer import GamePlayerSTOII
from utils.ggp.action import Action
from utils.ggp.jointaction import JointAction
from utils.ggp.state import State


class OpenMCTSNodeSTOII:
    def __init__(self, parent, parent_action):
        self.parent: OpenMCTSNodeSTOII = parent
        self.parent_action: Action = parent_action
        self.children: Dict[JointAction, OpenMCTSNodeSTOII] = dict()

        # --- STATS --- #
        self.nb_visit: int = 0  # nb of times the node was visited
        self.total_goal: float = 0  # sum of goal values the node has resulted in

    def get_child(self, action):
        return self.children[action]

    def add_child(self, action):
        self.children[action] = OpenMCTSNodeSTOII(parent=self, parent_action=action)
        return self.children[action]

    def update_edges(self, actions):
        for a in actions:
            if a not in self.children:
                self.children[a] = None

    def filter_children(self, actions):
        self.update_edges(actions)
        actions_to_delete = list()
        for a in self.children:
            if a not in actions:
                actions_to_delete.append(a)
        for a in actions_to_delete:
            del self.children[a]

    def unexplored_edges(self, legal_actions):
        return list(filter(lambda a: a in legal_actions and self.children[a] is None, self.children))

    def explored_children(self):
        return list(filter(lambda node: node is not None, self.children.values()))

    def explored_children_from_set(self, legal_actions):
        return list(filter(lambda node: node is not None and node.parent_action in legal_actions, self.children.values()))

    def has_unexplored_children(self):
        for child in self.children.values():
            if child is None:
                return True
        return False

    def UCB1(self, bias):
        if self.nb_visit != 0 and self.parent is not None:
            return (self.total_goal / self.nb_visit) + bias * sqrt(log(self.parent.nb_visit) / self.nb_visit)
        else:
            return 0

    def AVG(self):
        if self.nb_visit != 0:
            return self.total_goal / self.nb_visit
        else:
            return 0

    def children_maxUCB1(self, bias, legal_actions):
        explored_children = self.explored_children_from_set(legal_actions)
        if len(explored_children) > 0:
            maxvalue = max([c.UCB1(bias) for c in explored_children])
            return [c for c in explored_children if c.UCB1(bias) == maxvalue]
        return list()

    def children_maxAVG(self):
        explored_children = self.explored_children()
        if len(explored_children) > 0:
            maxvalue = max([c.AVG() for c in explored_children])
            return [c for c in explored_children if c.AVG() == maxvalue]
        return list()

    def __repr__(self):
        return str(f"AVG:{self.AVG():.2f}\t UCB1:{self.UCB1(math.sqrt(2)):.2f}")


def OpenMCTSSTOIINode_fromNodes(nodes: List[OpenMCTSNodeSTOII]):
    combined_node = OpenMCTSNodeSTOII(parent=None, parent_action=None)
    for node in nodes:
        combined_node.parent_stochastic_action = node.parent_action
        combined_node.children = merge_children(combined_node.children, node.children)
        combined_node.nb_visit += node.nb_visit
        combined_node.total_goal += node.total_goal
    return combined_node


def merge_children(children, new_children):
    for key in new_children:
        if key not in children:
            children[key] = new_children[key]
        else:
            nodes = list()
            if children[key] is not None:
                nodes.append(children[key])
            if new_children[key] is not None:
                nodes.append(new_children[key])
            children[key] = OpenMCTSSTOIINode_fromNodes(nodes)
    return children


class OpenMCTSPlayerSTOII(GamePlayerSTOII):
    def __init__(self, port, expl_bias=math.sqrt(2)):
        super().__init__(port)
        self.root_node: OpenMCTSNodeSTOII = None  # root node of the game tree
        self.root_states: List[State] = list()  # possible root states of the game tree
        self.state: State = None  # current state in the tree traversal
        self.legal_actions: List[Action] = None  # list of legal sas in self.state (empty if terminal)

        self.action_hist = list()
        self.outcome_hist = list()
        self.percept_hist = list()

        self.firstRound = True
        self.terminal = False

        self.expl_bias: float = expl_bias  # exploration bias to use with UCB1
        self.rounds_per_loop: int = 50  # simulation rounds per expansion

    def init_gametree(self):
        self.root_node: OpenMCTSNodeSTOII = None  # root node of the game tree
        self.root_states: List[State] = list()  # possible root states of the game tree
        self.state: State = None  # current state in the tree traversal
        self.legal_actions: List[Action] = None  # list of legal sas in self.state (empty if terminal)

        self.action_hist = list()
        self.outcome_hist = list()
        self.percept_hist = list()

        self.firstRound = True
        self.terminal = False

    def make_node(self, parent, action):
        return OpenMCTSNodeSTOII(parent=parent, parent_action=action)

    @stopit.threading_timeoutable()
    def player_start(self):
        self.root_node = self.make_node(parent=None, action=None)
        self.root_states.append(self.simulator.initial_state())
        legal_actions = self.simulator.legal_actions_from_states(self.root_states, self.role)
        self.root_node.update_edges(legal_actions)

    @stopit.threading_timeoutable()
    def player_play(self, *args, **kwargs):
        if not self.firstRound:
            self.action_hist.append(args[0])
            self.outcome_hist.append(args[1])
            self.percept_hist.append(args[2])
            self.update_root()
        rounds = 0
        while True:
            try:
                self.pick_random_root()
                node = self.select(self.root_node)
                node = self.expand(node)
                goal_value = self.simulate(rounds=self.rounds_per_loop)
                self.backprop(node, goal_value, visits=self.rounds_per_loop)
                rounds += self.rounds_per_loop
            except stopit.TimeoutException:
                print(f"Ran {rounds} rounds this cycle distributed over {len(self.root_states)} root states")
                return self.action_choice()

    def update_root(self):
        self.root_node = self.root_node.get_child(self.action_hist[-1])
        if self.root_node is None:
            self.root_node = self.make_node(None, None)
        self.root_states = self.simulator.update_states_stoii(self.root_states,
                                                              self.action_hist[-1],
                                                              self.outcome_hist[-1],
                                                              self.percept_hist[-1],
                                                              self.terminal)
        legal_actions = self.simulator.legal_actions_from_states(self.root_states, self.role)
        self.root_node.filter_children(legal_actions)
        print(f"{self.role} currently has {len(self.root_states)} root states")

    def pick_random_root(self):
        self.state = random.choice(self.root_states)
        self.legal_actions = self.simulator.legal_actions(self.state, self.role)

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        self.action_hist.append(args[0])
        self.outcome_hist.append(args[1])
        self.percept_hist.append(args[2])
        self.update_root()
        avg_goal = self.simulator.avg_goal(self.role, self.root_states)
        self.init_gametree()
        return avg_goal

    def action_choice(self):
        best_children = self.root_node.children_maxAVG()
        return random.choice(best_children).parent_action


    # ---------------------------------------------------------------------------------------------------------------- #
    # ------------------------------------------------    PHASES    -------------------------------------------------- #
    # ---------------------------------------------------------------------------------------------------------------- #
    def select(self, node):
        while not node.has_unexplored_children() and not self.simulator.is_terminal(self.state):
            node = random.choice(node.children_maxUCB1(self.expl_bias, self.legal_actions))
            self.update_state(node.parent_action)
            node.update_edges(self.legal_actions)
        return node  # node with unexplored edges or terminal node

    def expand(self, node):
        unexplored_actions = node.unexplored_edges(self.legal_actions)
        if len(unexplored_actions) > 0:
            action = unexplored_actions[0]  # pick an action
            node = node.add_child(action)
            self.update_state(action)
            return node
        return node  # terminal node

    def simulate(self, rounds):
        return self.simulator.simulate_sto(self.state, self.role, rounds)

    def backprop(self, node, value, visits):
        if node is not None:
            node.nb_visit += visits
            node.total_goal += value
            self.backprop(node.parent, value, visits)

    def update_state(self, action):
        stochastic_jointaction = self.simulator.complete_jointaction(self.state, action) #add random actions for other players
        deterministic_jointaction = self.simulator.sample_jointaction_outcome(self.state, stochastic_jointaction) #sample outcomes
        self.state = self.simulator.next_state(self.state, deterministic_jointaction)
        self.legal_actions = self.simulator.legal_actions(self.state, self.role)


