import math
import random
from typing import Dict
import numpy as np
import stopit
from math import sqrt, log
from gameplayer import GamePlayerSTO
from utils.ggp.jointaction import JointAction
from utils.ggp.state import State


class ChanceNode:
    def __init__(self, stochastic_jointaction, deterministic_jointactions):
        self.stochastic_jointaction = stochastic_jointaction
        self.children: Dict[JointAction, MCTSNodeSTO] = dict()
        self.children_probs: Dict[JointAction, float] = dict()
        for dja, prob in deterministic_jointactions.items():
            self.children[dja] = None
            self.children_probs[dja] = prob

        self.unexplored = True

    def get_child(self, deterministic_jointaction):
        return self.children[deterministic_jointaction]

    def UCB1(self, bias):
        """Sum of UCB1 of the child nodes weighted by their probability"""
        return sum(list(map(lambda dja: self.children_probs[dja] * self.children[dja].UCB1(bias), self.children)))

    def AVG(self):
        """Sum of AVG of the child nodes weighted by their probability"""
        return sum(list(map(lambda dja: self.children_probs[dja] * self.children[dja].AVG(), self.children)))

    def sample(self):
        return np.random.choice(list(self.children.values()), p=list(self.children_probs.values()))

    def __repr__(self):
        return str(f"AVG:{self.AVG():.2f}")


class MCTSNodeSTO:
    """MCTSNodeSTO represents a node in a closed-loop MCTS built game-tree that uses ChanceNodes to model stochastic
    action effects."""

    def __init__(self,
                 parent, parent_stochastic_jointaction, parent_deterministic_jointaction,
                 state,
                 child_jointactions: Dict[JointAction, Dict[JointAction, float]]):
        self.parent: MCTSNodeSTO = parent
        self.parent_stochastic_jointaction: JointAction = parent_stochastic_jointaction
        self.parent_deterministic_jointaction: JointAction = parent_deterministic_jointaction
        self.state: State = state
        self.children: Dict[JointAction, ChanceNode] = dict()  # Jointactions and their corresponding chance nodes

        for stochastic_jointaction in child_jointactions:
            self.children[stochastic_jointaction] = ChanceNode(stochastic_jointaction,
                                                               child_jointactions[stochastic_jointaction])

        # --- STATS --- #
        self.nb_visit: int = 1  # nb of times the node was visited
        self.total_goal: float = 1  # sum of goal values the node has resulted in

    def get_child(self, stochastic_jointaction, deterministic_jointaction):
        return self.children[stochastic_jointaction].get_child(deterministic_jointaction)

    def get_child_chancenode(self, stochastic_jointaction):
        return self.children[stochastic_jointaction]

    def unexplored_stochastic_jointactions(self):
        """Returns a list of all unexplored stochastic jointactions"""
        return list(filter(lambda ja: self.children[ja].unexplored, self.children))

    def explored_children(self):
        return list(filter(lambda chance_node: not chance_node.unexplored, self.children.values()))

    def has_unexplored_children(self):
        for child in self.children.values():
            if child.unexplored:
                return True
        return False

    def is_terminal(self):
        return len(self.children) == 0

    def UCB1(self, bias):
        if self.nb_visit != 0 and self.parent is not None:
            return (self.total_goal / self.nb_visit) + bias * sqrt(log(self.parent.nb_visit) / self.nb_visit)
        else:
            return 0

    def AVG(self):
        return self.total_goal / self.nb_visit \
            if self.nb_visit != 0 \
            else 0

    def children_maxUCB1(self, bias):
        explored_children = self.explored_children()
        if len(explored_children) > 0:
            maxvalue = max([c.UCB1(bias) for c in explored_children])
            return [c for c in explored_children if c.UCB1(bias) == maxvalue]
        return list()

    def children_maxAVG(self):
        explored_children = self.explored_children()
        if len(explored_children) > 0:
            values = [c.AVG() for c in explored_children]
            maxvalue = max(values)
            return [c for c in explored_children if c.AVG() == maxvalue]
        return list()

    def print(self, bias, level=0, last_level=20):
        if level <= last_level:
            print("\t" * level + self.stats(bias))
            for stochastic_child in self.explored_children():
                for deterministic_child in stochastic_child.children.values():
                    deterministic_child.print(bias, level + 1)

    def stats(self, bias):
        return f'{self.parent_stochastic_jointaction} + {self.parent_deterministic_jointaction}: ' \
               f'[UCB1={self.UCB1(bias):.2f},' \
               f' VISITS={self.nb_visit},' \
               f' AVG={self.AVG():.2f}] '

    def __repr__(self):
        return str(f"AVG:{self.AVG():.2f}")


class MCTSPlayerSTO(GamePlayerSTO):
    def __init__(self, port, expl_bias=math.sqrt(2)):
        super().__init__(port)
        self.root_node: MCTSNodeSTO = None  # root node of the game tree
        self.expl_bias: float = expl_bias  # exploration bias to use with UCB1
        self.rounds_per_loop: int = 2  # simulation rounds per expansion

    def make_node(self, parent, stochastic_jointaction, deterministic_jointaction, state=None):
        """ Generates an MCTS node with parent *parent* resulting from taking jointaction *jointaction* (which results
        in state *state*). """
        if state is None:
            state = self.simulator.next_state(parent.state, deterministic_jointaction)
        child_jointactions = self.simulator.legal_jointaction_sto(state)
        return MCTSNodeSTO(parent=parent,
                           parent_stochastic_jointaction=stochastic_jointaction,
                           parent_deterministic_jointaction=deterministic_jointaction,
                           state=state,
                           child_jointactions=child_jointactions)

    @stopit.threading_timeoutable()
    def player_start(self):
        self.root_node = self.make_node(parent=None,
                                        stochastic_jointaction=None,
                                        deterministic_jointaction=None,
                                        state=self.simulator.initial_state())

    @stopit.threading_timeoutable()
    def player_play(self, first_round: bool, *args, **kwargs):
        if not first_round:
            stochastic_jointaction: JointAction = args[0]
            deterministic_jointaction: JointAction = args[1]
            self.update_root_node(stochastic_jointaction, deterministic_jointaction)
        loops = 0
        while True:
            try:
                node = self.select(self.root_node)
                node = self.expand(node)
                goal_value = self.simulate(node, rounds=self.rounds_per_loop)
                self.backprop(node, goal_value, visits=self.rounds_per_loop)
                loops += self.rounds_per_loop
            except stopit.TimeoutException:
                print(f"loops {loops}")
                return self.action_choice()

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        stochastic_jointaction: JointAction = args[0]
        deterministic_jointaction: JointAction = args[1]
        self.update_root_node(stochastic_jointaction, deterministic_jointaction)
        return self.simulator.goal(self.root_node.state, self.role)

    def update_root_node(self, stochastic_jointaction, deterministic_jointaction):
        new_root = self.root_node.get_child(stochastic_jointaction, deterministic_jointaction)
        if new_root is None:
            state = self.simulator.next_state(self.root_node.state, deterministic_jointaction)
            self.root_node = self.make_node(parent=None,
                                            stochastic_jointaction=None,
                                            deterministic_jointaction=None,
                                            state=state)
        else:
            self.root_node = new_root
            self.root_node.parent = None

    def action_choice(self):
        best_children = self.root_node.children_maxAVG()
        return random.choice(best_children).stochastic_jointaction.get_action(self.role)

    # PHASE 1: Select best node to expand with UCB1 starting from a given node
    def select(self, node):
        while not node.has_unexplored_children() and not node.is_terminal():
            best_chancenodes = node.children_maxUCB1(self.expl_bias)
            node = random.choice(best_chancenodes).sample()
        return node

    # PHASE 2: Expand the given node
    def expand(self, node):
        """Generates all MCTSNodeSTO children of the first unexplored ChanceNode child of the given node.
        Returns one of the resulting MCTSNodeSTO children weighted by their probability."""
        stochastic_jointactions = node.unexplored_stochastic_jointactions()
        if len(stochastic_jointactions) > 0:
            stochastic_jointaction = stochastic_jointactions[0]
            chance_node = node.get_child_chancenode(stochastic_jointaction)
            for dja in chance_node.children:
                chance_node.children[dja] = self.make_node(parent=node,
                                                           stochastic_jointaction=stochastic_jointaction,
                                                           deterministic_jointaction=dja)
            chance_node.unexplored = False
            return chance_node.sample()
        return node  # terminal node

    # PHASE 3: Simulate a random game from the given node on
    def simulate(self, node, rounds=1):
        return self.simulator.simulate_sto(node.state, self.role, rounds)

    # PHASE 4: Backpropagate the terminal value through the ancestor nodes
    def backprop(self, node, value, visits=1):
        if node is not None:
            node.nb_visit += visits
            node.total_goal += value
            self.backprop(node.parent, value, visits)
