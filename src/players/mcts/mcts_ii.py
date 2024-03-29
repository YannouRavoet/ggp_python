import random
import math
import stopit
from gameplayer import GamePlayerII
from players.mcts.mcts import MCTSNode


class MCTSPlayerII(GamePlayerII):
    def __init__(self, port, expl_bias=math.sqrt(2)):
        super().__init__(port)
        self.root_nodes = None
        self.max_root_nodes = 100000  # max number of root_nodes to keep track of (chosen randomly)
        self.expl_bias = expl_bias
        self.max_rounds_per_loop = 100

        self.action_hist = list()
        self.percept_hist = list()

    def make_node(self, parent, jointaction, state=None):
        if state is None:
            state = self.simulator.next_state(parent.state, jointaction)
        child_jointactions = self.simulator.legal_jointactions(state)
        return MCTSNode(parent, jointaction, state, child_jointactions)

    def get_root_states(self):
        return [node.state for node in self.root_nodes]

    @stopit.threading_timeoutable()
    def player_start(self):
        self.root_nodes = [self.make_node(None, None, self.simulator.initial_state())]

    @stopit.threading_timeoutable()
    def player_play(self, *args, **kwargs):
        if not self.firstRound:
            self.action_hist.append(args[0])
            self.percept_hist.append(args[1])
            self.update_root_nodes()

        rounds_per_loop = max(int(self.max_rounds_per_loop / len(self.root_nodes)), 1)
        loops = 0
        while True:
            try:
                nodes = self.select(self.root_nodes)
                nodes = self.expand(nodes)
                goal_value = self.simulate(nodes, rounds=rounds_per_loop)
                self.backprop(nodes, goal_value, visits=rounds_per_loop)
                loops += rounds_per_loop * len(self.root_nodes)
            except stopit.TimeoutException:
                print(f"Ran {loops} MCTS loops...")
                return self.action_choice()

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        self.action_hist.append(args[0])
        self.percept_hist.append(args[1])
        return self.simulator.avg_goal_from_hist(self.action_hist, self.percept_hist, self.role)

    def update_root_nodes(self):
        """
        Updates the list of root_nodes by going over each direct child node and keeping those that have the correct
        action and percepts.
        :return: void
        """
        own_action = self.action_hist[-1]
        new_root_nodes = list()
        child_nodes = list()
        child_states = list()  # states before the actions are taken
        child_jointactions = list()
        for root_node in self.root_nodes:
            for ja, child_node in root_node.children.items():
                if ja.get_action(self.role) == own_action:
                    if child_node is None:
                        child_node = self.make_node(parent=root_node, jointaction=ja)
                    if not child_node.is_terminal():
                        child_nodes.append(child_node)
                        child_jointactions.append(ja)
                        child_states.append(root_node.state)

        child_node_indices = self.simulator.filter_states_percepts_ii(child_states,
                                                                      child_jointactions,
                                                                      self.role,
                                                                      self.percept_hist[-1])
        for i in child_node_indices:
            child_nodes[i].parent = None
            new_root_nodes.append(child_nodes[i])
        if len(new_root_nodes) > self.max_root_nodes:
            self.root_nodes = random.choices(new_root_nodes, k=self.max_root_nodes)
        else:
            self.root_nodes = new_root_nodes
        print(f"Currently {len(self.root_nodes)} root_nodes.")

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
        return random.choice(best_children).jointaction.get_action(self.role)

    # PHASE 1: Select best nodes to expand with UCB1 starting from a given nodes
    def select(self, nodes):
        selected = list()
        for node in nodes:
            while not node.has_unexplored_children() and not node.is_terminal():
                best_jointaction = max(node.children, key=lambda ja: node.children[ja].UCB1(self.expl_bias))
                node = node.get_child(best_jointaction)
            selected.append(node)
        return selected

    # PHASE 2: Expand the given nodes
    def expand(self, nodes):
        expanded = list()
        for node in nodes:
            jointactions = node.unexplored_jointactions()
            if len(jointactions) > 0:
                childnode = self.make_node(parent=node, jointaction=jointactions[0])
                node.children[jointactions[0]] = childnode
                expanded.append(childnode)
            else:
                expanded.append(node) # terminal node
        return expanded

    # PHASE 3: Simulate a random game from the given node on
    def simulate(self, nodes, rounds=1):
        states = [node.state for node in nodes]
        return self.simulator.simulate_states(states, self.role, rounds)

    # PHASE 4: Backpropagate the terminal value through the ancestor nodes
    def backprop_node(self, node, value, visits):
        if node is not None:
            node.nb_visit += visits
            node.total_goal += value
            self.backprop_node(node.parent, value, visits)

    def backprop(self, nodes, values, visits):
        for i in range(0, len(nodes)):
            self.backprop_node(nodes[i], values[i], visits)

