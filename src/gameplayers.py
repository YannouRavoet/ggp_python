import random
from math import sqrt, log

from gameplayer import GamePlayer, ClockOverException


class LegalPlayer(GamePlayer):
    """
    LegalPlayer selects the first legal action each round.
    """

    def calculate_action(self):
        legal_actions = self.match['Simulator'].legal_actions(self.match['State'], self.match['Role'])
        assert (len(legal_actions))
        return legal_actions[0]


class RandomPlayer(GamePlayer):
    """
    RandomPlayer selects a random legal action each round.
    """

    def calculate_action(self):
        legal_actions = self.match['Simulator'].legal_actions(self.match['State'], self.match['Role'])
        assert (len(legal_actions))
        return random.choice(legal_actions)


class MCTSPlayer(GamePlayer):
    """
    MCTSPlayer builds a game tree using Monte-Carlo Tree Search with Upper Confidence Bound 1 (UCB1).
    As a reminder the four phases of MCTS are (1) selection, (2) expansion, (3) simulation and (4) backpropagation.
    """

    class MCTSNode:
        """
        MCTSNode represents a node in a MCTS built game-tree.
        """

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
            if self.nb_visit != 0:
                return (self.total_goal / self.nb_visit) + bias * sqrt(log(self.parent.nb_visit) / self.nb_visit)
            else:
                return 0

    def __init__(self, name, port, expl_bias=2):
        super().__init__(name, port)
        self.expl_bias = expl_bias  # exploration bias to use with UCB1
        self.root_node = None  # root node of the game tree

    def make_node(self, parent, jointaction, state):
        child_jointactions = self.match['Simulator'].legal_jointaction_permutations(state)
        return self.MCTSNode(parent, jointaction, state, child_jointactions)

    """""""""""""""""""""
     GAMEPLAYER OVERRIDE
    """""""""""""""""""""
    def prepare(self):
        self.root_node = self.make_node(None, None, self.match['State'])

    def update_search_root(self, jointaction):
        self.root_node = self.root_node.children[jointaction]

    def calculate_action(self):
        # PHASE 1: Select best node to expand with UCB1 starting from a given node
        def select(node):
            while not node.has_unexplored_children() and not node.is_leaf():
                best_jointaction = max(node.children, key=lambda ja: node.children[ja].UCB1(self.expl_bias))
                node = node.get_child(best_jointaction)
            return node

        # PHASE 2: Expand the given node
        def expand(node):
            jointactions = node.unexplored_jointactions()
            if len(jointactions):
                childnode = self.make_node(node, jointactions[0], self.match['Simulator'].next_state(node.state, jointactions[0]))
                node.children[jointactions[0]] = childnode
                return childnode
            else:
                return node

        # PHASE 3: Simulate a random game from the given node on
        def simulate(node):
            state = node.state
            while not self.match['Simulator'].terminal(state):
                jointaction = random.choice(self.match['Simulator'].legal_jointaction_permutations(node.state))
                state = self.match['Simulator'].next_state(state, jointaction)
            return self.match['Simulator'].goal(state, self.match['Role'])

        # PHASE 4: Backpropagate the terminal value through the ancestor nodes
        def backprop(node, value):
            while node is not None:
                node.nb_visit += 1
                node.total_goal += value
                node = node.parent

        def action_choice():
            best_child = max(self.root_node.explored_children(), key=lambda c: c.UCB1(self.expl_bias))
            return best_child.jointaction.actions[self.match['Role']]

        while True:
            try:
                node = select(self.root_node)
                node = expand(node)
                goal_value = simulate(node)
                backprop(node, goal_value)

            except ClockOverException:
                self.match['Simulator'].engine.clear_stack()
                return action_choice()
