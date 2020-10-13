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
    Constant,
    Not
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
            return Var('_' + toks.variable[0])

        def statement_action(toks):
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

        if name == '<=':
            head = args[0]
            body = reduce(lambda b, a: a & b, reversed(args[1:]))
            return Clause(head, body)
        else:
            args = [Var(arg) for arg in args]
            if name == 'or':
                return Or(args[0], args[1])
            elif name == 'not':
                return Not('\+', args[0])
            elif name == 'distinct':
                return Term('\+', Term('=', *args))
            elif name == 'true':
                return args[0]
            return Term(name, *args)

    @staticmethod
    def _statements_action(toks):
        return list(toks.statements)


def read_rules(gdl_file):
    """
    Reads the rules of a GDL/KIF file into a string
    :param gdl_file: file location pf game rules in KIF format (Knowledge Interchange Format)
    :return: string of game rules in GDL format
    """
    with open(gdl_file) as f:
        gdl_rules = '\n'.join(line for line in (line.strip() for line in f.readlines())
                              if line and not line.startswith(';'))
    return gdl_rules


def parse_rules_to_string(gdl_rules):
    """
    Parses a given string of gdl_rules into a string of equivalent problog_rules.
    :param gdl_rules: string of game rules in KIF format (Knowledge Interchange Format)
    :return: string of game rules in ProbLog format
    """
    return term_list_to_string(parse_rules_term_list(gdl_rules))


def term_list_to_string(term_list):
    return '\n'.join([str(term) + '.' for term in term_list])


def parse_rules_term_list(gdl_rules):
    return list(GDLtoProbLogParser().statements.parseString(gdl_rules, parseAll=True))
