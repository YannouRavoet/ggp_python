import math
import random
from typing import Dict, List
import numpy as np
import stopit
from math import sqrt, log
from gameplayer import GamePlayerSTO
from utils.ggp.jointaction import JointAction
from utils.ggp.state import State


class OpenMCTSNodeSTO:
    """MCTSNodeSTO represents a node in a open-loop MCTS built game-tree. The node does not store the state of the game,
     since multiple states can be true at once."""

    def __init__(self, parent, parent_stochastic_jointaction):
        self.parent: OpenMCTSNodeSTO = parent
        self.parent_stochastic_jointaction: JointAction = parent_stochastic_jointaction
        self.children: Dict[JointAction, OpenMCTSNodeSTO] = dict()

        # --- STATS --- #
        self.nb_visit: int = 0  # nb of times the node was visited
        self.total_goal: float = 0  # sum of goal values the node has resulted in

    def get_child(self, stochastic_jointaction):
        return self.children[stochastic_jointaction]

    def add_child(self, stochastic_jointaction):
        self.children[stochastic_jointaction] = OpenMCTSNodeSTO(parent=self,
                                                                parent_stochastic_jointaction=stochastic_jointaction)

    def update_children(self, stochastic_jointactions):
        for sja in stochastic_jointactions:
            if sja not in self.children:
                self.children[sja] = None

    def filter_children(self, stochastic_jointactions):
        """Usefull when the root node is updated and we want to remove all sja's that were only valid in other states."""
        self.update_children(stochastic_jointactions)
        sjas_to_delete = list()
        for sja in self.children:
            if sja not in stochastic_jointactions:
                sjas_to_delete.append(sja)
        for sja in sjas_to_delete:
            del self.children[sja]

    def unexplored_stochastic_jointactions(self, legal_sjas):
        """Returns a list of all unexplored stochastic jointactions"""
        return list(filter(lambda sja: sja in legal_sjas
                                       and self.children[sja] is None, self.children))

    def explored_children(self, legal_sjas):
        return list(filter(lambda node: node.parent_stochastic_jointaction in legal_sjas
                                        and node is not None, self.children.values()))

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
        return self.total_goal / self.nb_visit \
            if self.nb_visit != 0 \
            else 0

    def children_maxUCB1(self, bias, legal_sjas):
        explored_children = self.explored_children(legal_sjas)
        if len(explored_children) > 0:
            maxvalue = max([c.UCB1(bias) for c in explored_children])
            return [c for c in explored_children if c.UCB1(bias) == maxvalue]
        return list()

    def children_maxAVG(self, legal_sjas):
        explored_children = self.explored_children(legal_sjas)
        if len(explored_children) > 0:
            maxvalue = max([c.AVG() for c in explored_children])
            return [c for c in explored_children if c.AVG() == maxvalue]
        return list()

    def __repr__(self):
        return str(f"AVG:{self.AVG():.2f}\t UCB1:{self.UCB1(math.sqrt(2))}")


class OpenMCTSPlayerSTO(GamePlayerSTO):
    def __init__(self, port, expl_bias=math.sqrt(2)):
        super().__init__(port)
        self.root_node: OpenMCTSNodeSTO = None  # root node of the game tree
        self.root_state: State = None  # root state of the game tree
        self.state: State = None  # current state in the tree traversal
        self.legal_sjas: List[JointAction] = None  # list of legal sjas in self.state (empty if terminal)

        self.expl_bias: float = expl_bias  # exploration bias to use with UCB1
        self.rounds_per_loop: int = 1  # simulation rounds per expansion

    def make_node(self, parent, stochastic_jointaction):
        """ Generates an MCTS node with parent *parent* resulting from taking jointaction *jointaction* (which results
        in state *state*). """
        return OpenMCTSNodeSTO(parent=parent, parent_stochastic_jointaction=stochastic_jointaction)

    @stopit.threading_timeoutable()
    def player_start(self):
        self.root_node = self.make_node(parent=None, stochastic_jointaction=None)
        self.root_state = self.simulator.initial_state()
        sjas = self.simulator.legal_jointactions(self.root_state)
        self.root_node.update_children(sjas)
        self.state = self.root_state
        self.legal_sjas = sjas

    @stopit.threading_timeoutable()
    def player_play(self, first_round: bool, *args, **kwargs):
        if not first_round:
            stochastic_jointaction: JointAction = args[0]
            deterministic_jointaction: JointAction = args[1]
            self.update_root_node(stochastic_jointaction, deterministic_jointaction)
            self.state = self.root_state
        while True:
            try:
                node = self.select(self.root_node)
                node = self.expand(node)
                goal_value = self.simulate(node, rounds=self.rounds_per_loop)
                self.backprop(node, goal_value, visits=self.rounds_per_loop)
                self.reset_state_to_root()
            except stopit.TimeoutException:
                self.reset_state_to_root()
                return self.action_choice()

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        stochastic_jointaction: JointAction = args[0]
        deterministic_jointaction: JointAction = args[1]
        self.update_root_node(stochastic_jointaction, deterministic_jointaction)
        return self.simulator.goal(self.root_state, self.role)

    def update_root_node(self, stochastic_jointaction, deterministic_jointaction):
        self.root_state = self.simulator.next_state(self.root_state, deterministic_jointaction)
        self.legal_sjas = self.simulator.legal_jointactions(self.root_state)
        new_root = self.root_node.get_child(stochastic_jointaction)
        if new_root is None:
            self.root_node = self.make_node(parent=None,
                                            stochastic_jointaction=None)
            self.root_node.update_children(self.legal_sjas)
        else:
            self.root_node = new_root
            self.root_node.parent = None
            self.root_node.filter_children(self.legal_sjas)

    def update_state(self, stochastic_jointaction):
        dja: JointAction = self.simulator.sample_jointaction_outcome(self.state, stochastic_jointaction)
        self.state = self.simulator.next_state(self.state, dja)
        self.legal_sjas = self.simulator.legal_jointactions(self.state)

    def reset_state_to_root(self):
        self.state = self.root_state
        self.legal_sjas = self.simulator.legal_jointactions(self.state)

    def action_choice(self):
        best_children = self.root_node.children_maxAVG(self.legal_sjas)
        return random.choice(best_children).parent_stochastic_jointaction.get_action(self.role)

    # PHASE 1: Select best node to expand with UCB1 starting from a given node
    def select(self, node):
        while not node.has_unexplored_children() and not self.simulator.is_terminal(self.state):
            best_children = node.children_maxUCB1(self.expl_bias, self.legal_sjas)
            node = random.choice(best_children)
            self.update_state(node.parent_stochastic_jointaction)
            node.update_children(self.legal_sjas)
        return node

    # PHASE 2: Expand the given node
    def expand(self, node):
        """Generates all MCTSNodeSTO children of the first unexplored ChanceNode child of the given node.
        Returns one of the resulting MCTSNodeSTO children weighted by their probability."""

        unexplored_sjas = node.unexplored_stochastic_jointactions(self.legal_sjas)
        if len(unexplored_sjas) > 0:
            stochastic_jointaction = unexplored_sjas[0]
            child_node = self.make_node(node, stochastic_jointaction)
            node.children[stochastic_jointaction] = child_node
            self.update_state(stochastic_jointaction)
            return child_node
        return node  # terminal node

    # PHASE 3: Simulate a random game from the given node on
    def simulate(self, node, rounds=1):
        return self.simulator.simulate_sto(self.state, self.role, rounds)

    # PHASE 4: Backpropagate the terminal value through the ancestor nodes
    def backprop(self, node, value, visits=1):
        if node is not None:
            node.nb_visit += visits
            node.total_goal += value
            self.backprop(node.parent, value, visits)
