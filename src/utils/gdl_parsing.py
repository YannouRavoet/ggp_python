from functools import reduce
from pyparsing import (
    Forward,
    Group,
    Literal,
    Suppress,
    White,
    Word,
    ZeroOrMore,
    printables,
    quotedString,
)
from problog.logic import (
    Term,
    Clause,
    Var,
    Or,
    Not,
    Constant
)


class GDLParser():
    def __init__(self):
        super().__init__()
        self.lpar = Literal('(')('lpar')
        self.rpar = Literal(')')('rpar')
        self.qmark = Literal('?')('qmark')

        self.word_chars = ''.join(c for c in printables if c not in ('()'))
        self.word = Word(self.word_chars) | quotedString
        self.variable = (Suppress(self.qmark) + ~White() + self.word)('variable')

        self.constant = self.word('constant')

        self.term = Forward()
        self.terms = Group(ZeroOrMore(self.term))

        self.predicate_name = self.word('name')

        self.compound_term = Group(
            Suppress(self.lpar) +
            self.predicate_name +
            self.terms('arguments') +
            Suppress(self.rpar))('compound_term')

        self.term << (self.compound_term | self.variable | self.constant)

        self.statement = self.term('statement')
        self.statements = Group(ZeroOrMore(self.statement))('statements')


class GDLtoProbLogParser(GDLParser):
    def __init__(self):
        super().__init__()
        self.variables = {}

        def variable_action(toks):
            variable_name = toks.variable[0]
            try:
                # if the variable is already part of the statement yet, nothing has to be done
                return self.variables[variable_name]
            except KeyError:
                # else, add it
                var = Var('_'+variable_name)
                self.variables[variable_name] = var
                return var

        def statement_action(toks):
            # Reset variables after each statement
            self.variables = {}
            return toks.statement

        self.variable.addParseAction(variable_action)
        self.constant.addParseAction(self._constant_action)
        self.compound_term.addParseAction(self._compound_term_action)
        self.statement.addParseAction(statement_action)
        self.statements.addParseAction(self._statements_action)

    @staticmethod
    def _constant_action(toks):
        return Constant(toks.constant)

    @staticmethod
    def _compound_term_action(toks):
        name = toks.compound_term.name
        args = toks.compound_term.arguments

        # Translate GDL rule operator to Prolog rule operator
        if name == '<=':
            head = args[0]
            body = reduce(lambda b, a: a & b, reversed(args[1:]))
            return Clause(head, body)
        else:
            args = [Var(arg) for arg in args]
            if name == 'or':
                return Or(args[0], args[1])
            elif name == 'not':
                return Term('\+', args[0])
            elif name == 'distinct':
                return Term('\+', Term('=', *args))
            return Term(name, *args)

    @staticmethod
    def _statements_action(toks):
        statements = ''
        for statement in toks.statements:
            statements += str(statement) +'.\n'
        return statements


def parse_file(file):
    """
    Parses a GDL file into a ProbLog program represented by a string
    :param file: GDL file location
    :return: str
    """
    with open(file) as f:
        rules = '\n'.join(line for line in (line.strip() for line in f.readlines())
                          if line and not line.startswith(';'))
    return GDLtoProbLogParser().statements.parseString(rules, parseAll=True).statements
