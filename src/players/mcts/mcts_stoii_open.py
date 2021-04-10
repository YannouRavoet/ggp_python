import math
import random
from typing import Dict, List
import stopit
from math import sqrt, log
from gameplayer import GamePlayerSTOII
from utils.ggp.jointaction import JointAction
from utils.ggp.state import State


class OpenMCTSNodeSTOII:
    """OpenMCTSNodeSTOII represents a node in a open-loop MCTS built game-tree used in imperfect information games.
    An OpenMCTSNodeSTOII node does not store the state of the game, since it represents the set of states reached by
    the stochastic actions taken starting from one of the root-states."""

    def __init__(self, parent, parent_stochastic_jointaction):
        self.parent: OpenMCTSNodeSTOII = parent
        self.parent_stochastic_jointaction: JointAction = parent_stochastic_jointaction
        self.children: Dict[JointAction, OpenMCTSNodeSTOII] = dict()

        # --- STATS --- #
        self.nb_visit: int = 1  # nb of times the node was visited
        self.total_goal: float = 1  # sum of goal values the node has resulted in

    @classmethod
    def fromNodes(cls, nodes):
        combined_node = OpenMCTSNodeSTOII(parent=None, parent_stochastic_jointaction=None)
        for node in nodes:
            combined_node.nb_visit += node.nb_visit
            combined_node.total_goal += node.total_goal
        return combined_node

    def get_child(self, stochastic_jointaction):
        return self.children[stochastic_jointaction]

    def add_child(self, stochastic_jointaction):
        self.children[stochastic_jointaction] = OpenMCTSNodeSTOII(parent=self,
                                                                  parent_stochastic_jointaction=stochastic_jointaction)

    def get_children(self, role, stochastic_action):
        return list(filter(lambda node: node is not None
                                        and node.parent_stochastic_jointaction.get_action(role) == stochastic_action,
                           self.children.values()))

    """ Adds the sja as edges to the node if they were not present yet. """
    def update_edges(self, stochastic_jointactions):
        for sja in stochastic_jointactions:
            if sja not in self.children:
                self.children[sja] = None

    def filter_children(self, stochastic_jointactions):
        """ Useful when the root node is updated and we want to remove all sja's that were only valid in other
        states. """
        self.update_edges(stochastic_jointactions)
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

    def explored_children(self):
        return list(filter(lambda node: node is not None, self.children.values()))

    def explored_children_from_set(self, legal_sjas):
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
        explored_children = self.explored_children_from_set(legal_sjas)
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
        return str(f"AVG:{self.AVG():.2f}\t UCB1:{self.UCB1(math.sqrt(2))}")


class OpenMCTSPlayerSTOII(GamePlayerSTOII):
    def __init__(self, port, expl_bias=math.sqrt(2)):
        super().__init__(port)
        self.root_node: OpenMCTSNodeSTOII = None  # root node of the game tree
        self.root_states: List[State] = list()  # possible root states of the game tree
        self.state: State = None  # current state in the tree traversal
        self.legal_sjas: List[JointAction] = None  # list of legal sjas in self.state (empty if terminal)

        self.action_hist = list()
        self.outcome_hist = list()
        self.percept_hist = list()

        self.expl_bias: float = expl_bias  # exploration bias to use with UCB1
        self.rounds_per_loop: int = 50  # simulation rounds per expansion

        self.firstRound = True
        self.terminal = False

    def make_node(self, parent, stochastic_jointaction):
        """ Generates an MCTS node with parent *parent* resulting from taking jointaction *jointaction* (which results
        in state *state*). """
        return OpenMCTSNodeSTOII(parent=parent, parent_stochastic_jointaction=stochastic_jointaction)

    @stopit.threading_timeoutable()
    def player_start(self):
        self.root_node = self.make_node(parent=None, stochastic_jointaction=None)
        self.root_states.append(self.simulator.initial_state())
        sjas = self.simulator.legal_sjas_from_states(self.root_states)
        self.root_node.update_edges(sjas)

    @stopit.threading_timeoutable()
    def player_play(self, *args, **kwargs):
        if not self.firstRound:
            self.action_hist.append(args[0])
            self.outcome_hist.append(args[1])
            self.percept_hist.append(args[2])
            self.update_root()

        while True:
            try:
                self.pick_random_root()
                node = self.select(self.root_node)
                node = self.expand(node)
                goal_value = self.simulate(node, rounds=self.rounds_per_loop)
                self.backprop(node, goal_value, visits=self.rounds_per_loop)
            except stopit.TimeoutException:
                self.pick_random_root()
                return self.action_choice()

    """Filters out invalid nodes (from percepts) and updates state."""

    def update_root(self):
        root_nodes = self.root_node.get_children(self.role, self.action_hist[-1])
        self.root_node = OpenMCTSNodeSTOII.fromNodes(root_nodes)

        self.root_states = self.simulator.update_states_stoii(self.root_states,
                                                              self.action_hist[-1],
                                                              self.outcome_hist[-1],
                                                              self.percept_hist[-1],
                                                              self.terminal)
        sjas = self.simulator.legal_sjas_from_states(self.root_states)
        self.root_node.filter_children(sjas)

    def pick_random_root(self):
        self.state = random.choice(self.root_states)
        self.legal_sjas = self.simulator.legal_jointactions(self.state)

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        self.action_hist.append(args[0])
        self.outcome_hist.append(args[1])
        self.percept_hist.append(args[2])
        self.update_root()
        return self.simulator.avg_goal(self.role, self.root_states)

    def action_choice(self):
        best_children = self.root_node.children_maxAVG()
        return random.choice(best_children).parent_stochastic_jointaction.get_action(self.role)

    # ---------------------------------------------------------------------------------------------------------------- #
    # ------------------------------------------------    PHASES    -------------------------------------------------- #
    # ---------------------------------------------------------------------------------------------------------------- #
    # PHASE 1: Select best node to expand with UCB1 starting from a given node
    def select(self, node):
        while not node.has_unexplored_children() and not self.simulator.is_terminal(self.state):
            best_children = node.children_maxUCB1(self.expl_bias, self.legal_sjas)
            node = random.choice(best_children)
            self.update_state(node.parent_stochastic_jointaction)
            node.update_edges(self.legal_sjas)
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

    def update_state(self, stochastic_jointaction):
        dja: JointAction = self.simulator.sample_jointaction_outcome(self.state, stochastic_jointaction)
        self.state = self.simulator.next_state(self.state, dja)
        self.legal_sjas = self.simulator.legal_jointactions(self.state)

    # PHASE 3: Simulate a random game from the given node on
    def simulate(self, _, rounds=1):
        return self.simulator.simulate_sto(self.state, self.role, rounds)

    # PHASE 4: Backpropagate the terminal value through the ancestor nodes
    def backprop(self, node, value, visits=1):
        if node is not None:
            node.nb_visit += visits
            node.total_goal += value
            self.backprop(node.parent, value, visits)
