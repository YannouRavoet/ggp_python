import random
from typing import Dict

import stopit
from math import sqrt, log
from gameplayer import GamePlayer
from utils.ggp.jointaction import JointAction
from utils.ggp.state import State


class MCTSNode:
    """MCTSNode represents a node in a MCTS built game-tree."""

    def __init__(self, parent, parent_jointaction, state, child_jointactions):
        self.parent: MCTSNode = parent  # parent node in the game tree
        self.jointaction: JointAction = parent_jointaction  # JointAction made from parent to reach this node
        self.state: State = state  # State of this node
        self.nb_visit: int = 0  # nb of times the node was visited
        self.total_goal: float = 0  # sum of goal values the node has resulted in
        self.children: Dict[JointAction, MCTSNode] = dict()  # Jointactions and their corresponding child nodes

        for joint_action in child_jointactions:
            self.children[joint_action] = None

    def get_child(self, jointaction):
        if isinstance(jointaction, list):
            return None
        return self.children[jointaction]

    def explored_children(self):
        return list(filter(lambda c: c is not None, self.children.values()))

    def unexplored_jointactions(self):
        return list(filter(lambda ja: self.children[ja] is None, self.children))

    def has_unexplored_children(self):
        for child in self.children.values():
            if child is None:
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
            maxvalue = max([c.AVG() for c in explored_children])
            return [c for c in explored_children if c.AVG() == maxvalue]
        return list()

    def print(self, bias, level=0, last_level=1):
        if level <= last_level:
            print("\t" * level + self.stats(bias))
            for child in self.explored_children():
                child.print(bias, level + 1)

    def stats(self, bias):
        return f'{self.jointaction}: ' \
               f'[UCB1={self.UCB1(bias):.2f},' \
               f' VISITS={self.nb_visit},' \
               f' AVG={self.AVG():.2f}] '

    def __repr__(self):
        return str(f"AVG:{self.AVG():.2f} - STATE:{str(self.state)}")


class MCTSPlayer(GamePlayer):
    """MCTSPlayer builds a game tree using Monte-Carlo Tree Search with Upper Confidence Bound 1 (UCB1)."""

    def __init__(self, port, expl_bias=2):
        super().__init__(port)
        self.root_node: MCTSNode = None  # root node of the game tree
        self.expl_bias: float = expl_bias  # exploration bias to use with UCB1
        self.rounds_per_loop: int = 100  # simulation rounds per expansion

    def make_node(self, parent, jointaction, state=None):
        if state is None:
            state = self.simulator.next_state(parent.state, jointaction)
        child_jointactions = self.simulator.legal_jointactions(state)
        return MCTSNode(parent, jointaction, state, child_jointactions)

    @stopit.threading_timeoutable()
    def player_start(self):
        self.root_node = self.make_node(None, None, state=self.simulator.initial_state())

    @stopit.threading_timeoutable()
    def player_play(self, first_round, *args, **kwargs):
        if not first_round:
            jointaction = args[0]
            self.update_root_node(jointaction)

        loops = 0
        while True:
            try:
                node = self.select(self.root_node)
                node = self.expand(node)
                goal_value = self.simulate(node, rounds=self.rounds_per_loop)
                self.backprop(node, goal_value, visits=self.rounds_per_loop)
                loops += self.rounds_per_loop
            except stopit.TimeoutException:
                print(f"Ran {loops} loops of MCTS")
                self.root_node.print(self.expl_bias)
                return self.action_choice()

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        jointaction = args[0]
        self.update_root_node(jointaction)
        return self.simulator.goal(self.root_node.state, self.role)

    def update_root_node(self, jointaction):
        if self.root_node.get_child(jointaction) is None:  # child was not expanded yet
            state = self.simulator.next_state(self.root_node.state, jointaction)
            self.root_node = self.make_node(parent=None, jointaction=None, state=state)
        else:
            self.root_node = self.root_node.get_child(jointaction)
            self.root_node.parent = None    # remove parent to stop backtracking at new root

    def action_choice(self):
        best_children = self.root_node.children_maxAVG()
        return random.choice(best_children).jointaction.get_action(self.role)

    # PHASE 1: Select best node to expand with UCB1 starting from a given node
    def select(self, node):
        while not node.has_unexplored_children() and not node.is_terminal():
            best_childnodes = node.children_maxUCB1(self.expl_bias)
            node = random.choice(best_childnodes)
        return node

    # PHASE 2: Expand the given node
    def expand(self, node):
        jointactions = node.unexplored_jointactions()
        if len(jointactions) > 0:
            childnode = self.make_node(parent=node, jointaction=jointactions[0])
            node.children[jointactions[0]] = childnode
            return childnode
        return node  # terminal node

    # PHASE 3: Simulate a random game from the given node on
    def simulate(self, node, rounds=1):
        return self.simulator.simulate(node.state, self.role, rounds)

    # PHASE 4: Backpropagate the terminal value through the ancestor nodes
    def backprop(self, node, value, visits=1):
        if node is not None:
            node.nb_visit += visits
            node.total_goal += value
            self.backprop(node.parent, value, visits)
