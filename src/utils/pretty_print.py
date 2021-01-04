import os
import re
from copy import deepcopy


class Grid:
    class GridCell:
        def __init__(self, x, y, value):
            self.x = x  # col
            self.y = y  # row
            self.value = value if value is not None else '.'

        def print(self):
            print(f' {self.value} ', end='')

        def __repr__(self):
            return str(f'{self.x}, {self.y}: {self.value}')

        def __eq__(self, other):
            return self.x == other.x and self.y == other.y

        def __hash__(self):
            return hash(str(self.x) + str(self.y))

    def __init__(self, range_x, range_y, row_div_mod=1, col_div_mod=1):
        """ Builds a grid with the following index structure
                 range_x = range_y = 4
                   +---+---+---+---+
                 0 | . | . | . | . |
                   +---+---+---+---+
                 1 | . | . | . | . |
              Y    +---+---+---+---+
                 2 | . | . | . | . |
                   +---+---+---+---+
                 3 | . | . | . | . |
                   +---+---+---+---+
                     0   1   2   3
                             X
        """
        self.range_x = range_x
        self.range_y = range_y
        self.cells = list()

    def empty_grid(self):
        self.cells.clear()
        for y in range(0, self.range_y):
            for x in range(0, self.range_x):
                self.cells.append(self.GridCell(x, y, None))

    def _cell_index(self, x, y):
        return y * self.range_x + x

    def set_cell_value(self, x, y, val):
        indx = self._cell_index(x, y)
        self.cells[indx].value = val

    def print(self):
        def hor_bar(range_x):
            print('+---' * (range_x) + '+')

        def ver_bar(end=''):
            print('|', end=end)

        cells = deepcopy(self.cells)
        prev_y = -1
        while len(cells) > 0:
            cell = cells.pop(0)
            if cell.y != prev_y:
                hor_bar(self.range_x)
                prev_y = cell.y
            ver_bar()
            cell.print()
            if cell.x == self.range_x - 1:
                ver_bar(end='\n')
        hor_bar(self.range_x)


class PrettyPrinter:
    def print_state(self, state):
        self._clear()
        self._print(state)

    @staticmethod
    def _clear():
        os.system('clear')

    def _print(self, state):
        print(state.sort())


class Board2DPrinter(PrettyPrinter):
    def __init__(self, range_x, range_y, empty_val):
        self.grid = Grid(range_x, range_y)
        self.empty_val = empty_val  # the string used in the game_rules to represent an empty cell

    def _print(self, state):
        self.grid.empty_grid()
        non_cell_facts = list()
        for fact in state.facts:
            m = re.match(r"cell\(([0-9]*), ([0-9]*), (\w*)\)", fact)
            if m is None:
                m = re.match(r"location\((\w*), ([0-9]*), ([0-9]*)\)", fact)
                if m is not None:
                    self.parse_cell_term(int(m.group(2)), int(m.group(3)), m.group(1)[0].upper())
                else:
                    non_cell_facts.append(fact)
            elif m.group(3) != self.empty_val:
                self.parse_cell_term(int(m.group(1)), int(m.group(2)), m.group(3)[0].upper())
        self.grid.print()
        [print(fact) for fact in non_cell_facts]

    def parse_cell_term(self, x, y, val):
        self.grid.set_cell_value(x, y, val)


class PrettyPrinterFactory:
    @staticmethod
    def make_printer(gamefile) -> PrettyPrinter:
        if gamefile == 'maze.gdl':
            return Board2DPrinter(range_x=5, range_y=5, empty_val=None)
        if gamefile == 'mazemedium.gdl':
            return Board2DPrinter(range_x=9, range_y=7, empty_val=None)
        if gamefile == 'mazebig.gdl':
            return Board2DPrinter(range_x=15, range_y=10, empty_val=None)
        if gamefile in ['tictactoe.gdl', 'kriegtictactoe.gdl']:
            return Board2DPrinter(range_x=3, range_y=3, empty_val='b')
        if gamefile == 'connectfour.gdl':
            return Board2DPrinter(range_x=8, range_y=6, empty_val=None)
        return PrettyPrinter()
