import math
import random
from typing import Dict, List
import numpy as np
import stopit
from math import sqrt, log
from gameplayer import GamePlayerSTOII
from utils.ggp.jointaction import JointAction
from utils.ggp.state import State


class ChanceNode:
    def __init__(self, stochastic_jointaction, deterministic_jointactions):
        self.stochastic_jointaction = stochastic_jointaction
        self.children: Dict[JointAction, MCTSNodeSTOII] = dict()
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

    def combine(self, other):
        if other is None:
            return self
        for dja in self.children:
            if self.children[dja] is None:
                self.children[dja] = other.children[dja]
            else:
                self.children[dja].combine(other.children[dja])


class MCTSNodeSTOII:
    def __init__(self,
                 parent, parent_stochastic_jointaction, parent_deterministic_jointaction,
                 state,
                 child_jointactions: Dict[JointAction, Dict[JointAction, float]]):
        self.parent: MCTSNodeSTOII = parent
        self.parent_stochastic_jointaction: JointAction = parent_stochastic_jointaction
        self.parent_deterministic_jointaction: JointAction = parent_deterministic_jointaction
        self.state: State = state
        self.children: Dict[JointAction, ChanceNode] = dict()  # Jointactions and their corresponding chance nodes

        for stochastic_jointaction in child_jointactions:
            self.children[stochastic_jointaction] = ChanceNode(stochastic_jointaction,
                                                               child_jointactions[stochastic_jointaction])

        # --- STATS --- #
        self.nb_visit: int = 0  # nb of times the node was visited
        self.total_goal: float = 0  # sum of goal values the node has resulted in

    def get_all_children(self):  # returns all child decision nodes
        all_children = list()
        for child in self.children.values():
            all_children.extend(child.children.values())
        return all_children

    def add_children_to_dict(self, state_node_dict):
        for chance_node in self.children.values():
            for decision_node in chance_node.children.values():
                if decision_node is not None and decision_node.state in state_node_dict.keys():
                    state_node_dict[decision_node.state].append(decision_node)

    def get_child(self, stochastic_jointaction, deterministic_jointaction):
        return self.get_child_chancenode(stochastic_jointaction).get_child(deterministic_jointaction)

    def get_child_chancenode(self, stochastic_jointaction):
        return self.children[stochastic_jointaction]

    def unexplored_edges(self):
        return list(filter(lambda sja: self.children[sja].unexplored, self.children))

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
        if self.nb_visit != 0:
            return self.total_goal / self.nb_visit
        else:
            return 0

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

    def combine(self, other):
        if other is None:
            return self
        self.nb_visit += other.nb_visit
        self.total_goal += other.total_goal
        for sja in self.children:
            self.children[sja].combine(other.children[sja])



class MCTSPlayerSTOII(GamePlayerSTOII):
    def __init__(self, port, expl_bias=math.sqrt(2)):
        super().__init__(port)
        self.root_nodes: List[MCTSNodeSTOII] = None  # list of possible root nodes

        self.expl_bias: float = expl_bias  # exploration bias to use with UCB1
        self.rounds_per_loop: int = 50  # simulation rounds per expansion

        self.action_hist = list()
        self.outcome_hist = list()
        self.percept_hist = list()

        self.firstRound = True
        self.terminal = False

    def init_gametree(self):
        self.root_nodes: List[MCTSNodeSTOII] = None  # list of possible root nodes

        self.action_hist = list()
        self.outcome_hist = list()
        self.percept_hist = list()

        self.firstRound = True
        self.terminal = False

    def make_node(self, parent, stochastic_jointaction, deterministic_jointaction, state):
        child_jointactions = self.simulator.legal_jointaction_sto(state)
        return MCTSNodeSTOII(parent=parent,
                             parent_stochastic_jointaction=stochastic_jointaction,
                             parent_deterministic_jointaction=deterministic_jointaction,
                             state=state,
                             child_jointactions=child_jointactions)

    @stopit.threading_timeoutable()
    def player_start(self):
        self.root_nodes = [self.make_node(parent=None,
                                          stochastic_jointaction=None,
                                          deterministic_jointaction=None,
                                          state=self.simulator.initial_state())]

    @stopit.threading_timeoutable()
    def player_play(self, *args, **kwargs):
        if not self.firstRound:
            self.action_hist.append(args[0])
            self.outcome_hist.append(args[1])
            self.percept_hist.append(args[2])
            self.update_root_nodes()
        rounds = 0
        while True:
            try:
                for node in self.root_nodes:
                    node = self.select(node)
                    node = self.expand(node)
                    goal_value = self.simulate(node, rounds=self.rounds_per_loop)
                    self.backprop(node, goal_value, visits=self.rounds_per_loop)
                    rounds += self.rounds_per_loop
            except stopit.TimeoutException:
                print(f"Ran {rounds} rounds this cycle distributed over {len(self.root_nodes)} game trees")
                return self.action_choice()

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        self.action_hist.append(args[0])
        self.outcome_hist.append(args[1])
        self.percept_hist.append(args[2])
        self.update_root_nodes()
        avg_goal = self.simulator.avg_goal(self.role, [node.state for node in self.root_nodes])
        self.init_gametree()
        return avg_goal

    def update_root_nodes(self):
        root_states = [node.state for node in self.root_nodes]
        new_root_states = self.simulator.update_states_stoii(root_states,
                                                             self.action_hist[-1],
                                                             self.outcome_hist[-1],
                                                             self.percept_hist[-1],
                                                             self.terminal)

        new_root_nodes = dict()
        for state in new_root_states:
            new_root_nodes[state] = list()
        for root_node in self.root_nodes:
            child_nodes = root_node.get_all_children()
            for child_node in child_nodes:
                if child_node is not None and child_node.state in new_root_states:
                    new_root_nodes[child_node.state].append(child_node)
        self.root_nodes = self.shallow_combine(new_root_nodes)
        print(f"{self.role} currently has {len(self.root_nodes)} root nodes")

    def shallow_combine(self, state_node_dict):
        new_root_nodes = list()
        for state, nodes in state_node_dict.items():
            if len(nodes) == 0:
                #new_root_nodes.append(self.make_node(None, None, None, state))
                continue
            new_node: MCTSNodeSTOII = nodes[0]
            new_node.parent = None
            new_node.nb_visit = sum(list(map(lambda n: n.nb_visit, nodes)))
            new_node.total_goal = sum(list(map(lambda n: n.total_goal, nodes)))
            new_root_nodes.append(new_node)
        return new_root_nodes

    def combine_nodes(self, state_node_dict):
        new_root_nodes = list()
        for state, nodes in state_node_dict.items():
            if len(nodes) == 0:
                new_root_nodes.append(self.make_node(None, None, None, state))
                continue
            new_node = nodes[0]
            for i in range(1, len(nodes)):
                new_node.combine(nodes[i])
            new_root_nodes.append(new_node)
        return new_root_nodes

    def action_choice(self):
        best_children = list()
        best_child_avg = - 1
        for root_node in self.root_nodes:
            children_nodes = root_node.children_maxAVG()
            if len(children_nodes) > 0:
                children_nodes_avg = children_nodes[0].AVG()
                if children_nodes_avg > best_child_avg:
                    best_children = children_nodes
                    best_child_avg = children_nodes_avg
                elif children_nodes_avg == best_child_avg:
                    best_children.extend(children_nodes)
        return random.choice(best_children).stochastic_jointaction.get_action(self.role)

    def select(self, node):
        while not node.has_unexplored_children() and not node.is_terminal():
            node = random.choice(node.children_maxUCB1(self.expl_bias)).sample()
        return node

    def expand(self, node):
        stochastic_jointactions = node.unexplored_edges()
        if len(stochastic_jointactions) > 0:
            stochastic_jointaction = stochastic_jointactions[0]
            chance_node = node.get_child_chancenode(stochastic_jointaction)
            for dja in chance_node.children:
                chance_node.children[dja] = self.make_node(parent=node,
                                                           stochastic_jointaction=stochastic_jointaction,
                                                           deterministic_jointaction=dja,
                                                           state=self.simulator.next_state(node.state, dja))
            chance_node.unexplored = False
            return chance_node.sample()
        return node  # terminal node

    def simulate(self, node, rounds):
        return self.simulator.simulate_sto(node.state, self.role, rounds)

    def backprop(self, node, value, visits):
        if node is not None:
            node.nb_visit += visits
            node.total_goal += value
            self.backprop(node.parent, value, visits)
