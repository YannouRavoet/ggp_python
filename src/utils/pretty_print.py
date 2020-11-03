from copy import deepcopy


class Grid:
    # TOP LEFT CORNER IS (1,1)
    def __init__(self, rows=0, cols=0):
        self.cells = list()
        self.rows = rows
        self.cols = cols
        for r in range(1, self.rows+1):
            for c in range(1, self.cols+1):
                self.cells.append(GridCell(r, c, '.'))

    def set_cellvalue(self, row, col, value):
        index = (row-1) * self.cols + col - 1
        self.cells[index].value = value

    def print(self):
        cells = deepcopy(self.cells)
        prev_row = 0
        while len(cells) > 0:
            cell = cells.pop(0)
            if cell.row != prev_row:
                print(f'\n{str("---")*self.cols}')
                prev_row = cell.row
            cell.print()
        print(f'\n{str("---")*self.cols}')


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


class PrettyPrinter:
    @staticmethod
    def printerClass(gamefile):
        if gamefile == 'tictactoe.gdl':
            return TicTacToePrinter
        if gamefile == 'connectfour.gdl':
            return ConnectFourPrinter
        else:
            return PrettyPrinter

    def print_state(self, state):
        print(state)


class TicTacToePrinter(PrettyPrinter):

    def print_state(self, state):
        grid = Grid(rows=3, cols=3)
        for fact in state.facts:
            if fact.arity == 3 and str(fact.args[2]) != 'b':
                grid.set_cellvalue(int(fact.args[0]),
                                   int(fact.args[1]),
                                   str(fact.args[2]))
        grid.print()


class ConnectFourPrinter(PrettyPrinter):
    def print_state(self, state):
        grid = Grid(rows=6, cols=8)
        for fact in state.facts:
            if fact.arity == 3:
                grid.set_cellvalue(7-int(fact.args[1]),
                                   int(fact.args[0]),
                                   'x' if str(fact.args[2]) == 'red' else 'o')
        grid.print()

