from copy import deepcopy
from math import ceil


class Grid:
    class GridCell:
        def __init__(self, row, col, value):
            self.row = row
            self.col = col
            self.value = value

        def print(self):
            print(f' {self.value} ', end='')

        def __repr__(self):
            return str(f'{self.row}, {self.col}: {self.value}')

        def __eq__(self, other):
            return self.row == other.row and self.col == other.col

        def __hash__(self):
            return hash(str(self.row) + str(self.col))

    def __init__(self, rows=0, cols=0, blank='.', row_div_mod=1, col_div_mod=1):
        # TOP LEFT CORNER IS (1,1)
        self.cells = list()
        self.rows = rows
        self.cols = cols
        self.blank = blank
        self.row_div_mod = row_div_mod
        self.col_div_mod = col_div_mod
        self.row_divider = f'\n{"---" * self.cols}{"-" * ceil(self.rows / self.col_div_mod - 1) if self.col_div_mod is not None else ""}'

    def set_cellvalue(self, row, col, value):
        index = (row - 1) * self.cols + col - 1
        self.cells[index].value = value

    def print(self):
        cells = deepcopy(self.cells)
        prev_row = 1
        while len(cells) > 0:
            cell = cells.pop(0)
            if cell.row != prev_row:
                prev_row = cell.row
                print(self.row_divider) if (self.row_div_mod == 1) or (cell.row % self.row_div_mod == 1) else print("")

            cell.print()
            if cell.col != self.cols and cell.col % self.col_div_mod == 0:
                print('|', end='')
        print("\n")

    def empty(self):
        self.cells.clear()
        for r in range(1, self.rows + 1):
            for c in range(1, self.cols + 1):
                self.cells.append(self.GridCell(r, c, self.blank))


class PrettyPrinterFactory:
    @staticmethod
    def make_printer(gamefile):
        if gamefile == 'tictactoe.gdl' or gamefile=='kriegtictactoe_v2.gdl':
            return BaseBoardPrinter(rows=3, cols=3)
        if gamefile == 'connectfour.gdl':
            return ConnectFourPrinter(rows=6, cols=8, row_div_mod=-1, col_div_mod=1)
        if gamefile == 'sudoku.gdl':
            return BaseBoardPrinter(rows=9, cols=9, row_div_mod=3, col_div_mod=3)
        return PrettyPrinter()


class PrettyPrinter:
    def print_state(self, state):
        print(state.sort())


class BaseBoardPrinter(PrettyPrinter):
    def __init__(self, rows, cols, empty_cell_char='b', row_div_mod=1, col_div_mod=1):
        self.grid = Grid(rows, cols, row_div_mod=row_div_mod, col_div_mod=col_div_mod)
        self.empty_cell_char = empty_cell_char

    def print_state(self, state):
        self.grid.empty()
        for fact in state.facts:
            if fact.functor == "cell" and str(fact.args[2]) != self.empty_cell_char:
                self.process_state_cell(fact)
        self.grid.print()

    def process_state_cell(self, fact):
        self.grid.set_cellvalue(int(fact.args[0]),
                                int(fact.args[1]),
                                str(fact.args[2])[0])


class ConnectFourPrinter(BaseBoardPrinter):
    def process_state_cell(self, fact):
        self.grid.set_cellvalue(7 - int(fact.args[1]),
                                int(fact.args[0]),
                                'x' if str(fact.args[2]) == 'red' else 'o')
