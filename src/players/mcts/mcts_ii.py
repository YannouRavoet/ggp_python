import stopit
from gameplayer import GamePlayerII
from players.mcts.mcts import MCTSNode
from utils.ggp.action import Action
from utils.ggp.percepts import Percepts


class MCTSPlayerII(GamePlayerII):
    def __init__(self, port, expl_bias=2):
        super().__init__(port)
        self.root_nodes = None
        self.expl_bias = expl_bias
        self.rounds_per_loop = 25

        self.action_hist = list()
        self.percept_hist = list()

    def make_node(self, parent, jointaction, state):
        child_jointactions = self.simulator.legal_jointactions(state)
        return MCTSNode(parent, jointaction, state, child_jointactions)

    def get_root_states(self):
        return [node.state for node in self.root_nodes]

    @stopit.threading_timeoutable()
    def player_start(self):
        self.root_nodes = [self.make_node(None, None, self.simulator.initial_state())]

    @stopit.threading_timeoutable()
    def player_play(self, first_round, *args, **kwargs):
        if not first_round:
            self.action_hist.append(Action(self.role, args[0]))
            self.percept_hist.append(Percepts(self.role, args[1]))
            self.update_root_nodes()

        while True:
            for root_node in self.root_nodes:
                try:
                    node = self.select(root_node)
                    node = self.expand(node)
                    goal_value = self.simulate(node, rounds=self.rounds_per_loop)
                    self.backprop(node, goal_value, visits=self.rounds_per_loop)
                except stopit.TimeoutException:
                    self.simulator.engine.clear_stack()
                    return self.action_choice()

    @stopit.threading_timeoutable()
    def player_stop(self, *args, **kwargs):
        self.action_hist.append(Action(self.role, args[0]))
        self.percept_hist.append(Percepts(self.role, args[1]))
        self.update_root_nodes()
        terminal_states = self.simulator.filter_terminal_states(self.get_root_states())
        goalsum = 0
        for state in terminal_states:
            goalsum += self.simulator.goal(state, self.role)
        return goalsum / len(terminal_states)

    def update_root_nodes(self):
        """
        Updates the list of root_nodes by going over each direct child node and keeping those that have the correct
        action and percepts.
        :return: void
        """
        new_root_nodes = list()
        baction = self.action_hist[-1]
        bpercepts = self.percept_hist[-1]
        for root_node in self.root_nodes:
            for ja, child_node in root_node.children.items():
                caction = ja.get_action(self.role)
                cpercepts = self.simulator.percepts(root_node.state, ja, self.role)
                if caction == baction and \
                        cpercepts == bpercepts:
                    new_root_nodes.append(child_node)
        print(f"Had {len(self.root_nodes)} root_nodes.")
        print(f"Action: {self.action_hist[-1]} - Percepts: {self.percept_hist[-1]}")
        self.root_nodes = new_root_nodes
        print(f"Now {len(self.root_nodes)} root_nodes.")

    def action_choice(self):
        best_children = list()
        for root_node in self.root_nodes:
            best_children.append(max(root_node.explored_children(), key=lambda c: c.AVG()))
        return best_children[0].jointaction.get_action(self.role)

    # PHASE 1: Select best node to expand with UCB1 starting from a given node
    def select(self, node):
        while not node.has_unexplored_children() and not node.is_leaf():
            best_jointaction = max(node.children, key=lambda ja: node.children[ja].UCB1(self.expl_bias))
            node = node.get_child(best_jointaction)
        return node

    # PHASE 2: Expand the given node
    def expand(self, node):
        jointactions = node.unexplored_jointactions()
        if len(jointactions) > 0:
            childnode = self.make_node(parent=node, jointaction=jointactions[0],
                                       state=self.simulator.next_state(node.state, jointactions[0]))
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