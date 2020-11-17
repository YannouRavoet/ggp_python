from math import sqrt, log
from gameplayer import GamePlayer
from utils.clocked_function import ClockOverException


class MCTSPlayer(GamePlayer):
    class MCTSNode:
        """MCTSNode represents a node in a MCTS built game-tree."""
        def __init__(self, parent, parent_jointaction, state, child_jointactions):
            self.parent = parent  # parent node in the game tree
            self.jointaction = parent_jointaction  # JointAction made from parent to reach this node
            self.state = state  # State of this node
            self.nb_visit = 0  # nb of times the node was visited
            self.total_goal = 0  # sum of goal values the node has resulted in

            self.children = dict()  # unexplored jointactions and their corresponding child nodes
            for joint_action in child_jointactions:
                self.children[joint_action] = None

        def get_child(self, jointaction):
            return self.children[jointaction]

        def expand(self, jointaction, childState, child_jointactions):
            child = self.__init__(parent=self,
                                  parent_jointaction=jointaction,
                                  state=childState,
                                  child_jointactions=child_jointactions)
            self.children[jointaction] = child
            return child

        def explored_children(self):
            return list(filter(lambda c: c is not None, self.children.values()))

        def unexplored_jointactions(self):
            return list(filter(lambda ja: self.children[ja] is None, self.children))

        def has_unexplored_children(self):
            for child in self.children.values():
                if child is None:
                    return True
            return False

        def is_leaf(self):
            return len(self.children) == 0

        def UCB1(self, bias):
            return (self.total_goal / self.nb_visit) + bias * sqrt(log(self.parent.nb_visit) / self.nb_visit) \
                if self.nb_visit != 0 and self.parent is not None \
                else 0

        def AVG(self):
            return self.total_goal/self.nb_visit \
                if self.nb_visit != 0 \
                else 0

        def print(self, bias, level=0, last_level=1):
            if level <= last_level:
                print("\t"*level + self.stats(bias))
                for child in self.explored_children():
                    child.print(bias, level+1)

        def stats(self, bias):
            return f'{self.jointaction}: ' \
                   f'[UCB1={self.UCB1(bias):.2f},' \
                   f' VISITS={self.nb_visit},' \
                   f' AVG={self.AVG():.2f}] '

    """MCTSPlayer builds a game tree using Monte-Carlo Tree Search with Upper Confidence Bound 1 (UCB1)."""
    def __init__(self, port, expl_bias=2):
        super().__init__(port)
        self.expl_bias = expl_bias  # exploration bias to use with UCB1
        self.root_node = None  # root node of the game tree

    def make_node(self, parent, jointaction, state):
        child_jointactions = self.simulator.legal_jointaction_permutations(state)
        return self.MCTSNode(parent, jointaction, state, child_jointactions)

    def player_start(self):
        self.root_node = self.make_node(None, None, self.simulator.initial_state())

    def player_play(self, *args, **kwargs):
        jointaction = args[0]
        self.update_root_node(jointaction)

        # MCTS FROM CURRENT STATE
        loops = 0
        rounds_per_loop = 100
        while True:
            try:
                node = self.select(self.root_node)
                node = self.expand(node)
                goal_value = self.simulate(node, rounds=rounds_per_loop)
                self.backprop(node, goal_value, visits=rounds_per_loop)
                loops += rounds_per_loop
            except ClockOverException:
                self.simulator.engine.clear_stack()
                print(f'ran {loops} MCTS loops')
                self.root_node.print(self.expl_bias, last_level=1)
                best_child = max(self.root_node.explored_children(), key=lambda c: c.AVG())
                return best_child.jointaction.get_action(self.role)

    def player_stop(self, *args, **kwargs):
        jointaction = args[0]
        self.update_root_node(jointaction)
        return self.simulator.goal(self.root_node.state, self.role)

    def update_root_node(self, jointaction):
        if len(jointaction) != 0:
            if self.root_node.children[jointaction] is None:  # child was not expanded yet
                self.root_node = self.make_node(parent=None,
                                                jointaction=None,
                                                state=self.simulator.next_state(self.root_node.state, jointaction))
            else:
                self.root_node = self.root_node.children[jointaction]
                self.root_node.parent = None
                self.root_node.jointaction = None

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
            childnode = self.make_node(parent=node,
                                       jointaction=jointactions[0],
                                       state=self.simulator.next_state(node.state, jointactions[0]))
            node.children[jointactions[0]] = childnode
            return childnode
        return node

    # PHASE 3: Simulate a random game from the given node on
    def simulate(self, node, rounds=1):
        return self.simulator.simulate(node.state, self.role, rounds)

    # PHASE 4: Backpropagate the terminal value through the ancestor nodes
    def backprop(self, node, value, visits=1):
        if node is not None:
            node.nb_visit += visits
            node.total_goal += value
            self.backprop(node.parent, value, visits)

