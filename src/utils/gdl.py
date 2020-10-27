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
        self.lpar = Literal('(')('lpar')
        self.rpar = Literal(')')('rpar')
        self.qmark = Literal('?')('qmark')

        self.word_chars = ''.join(c for c in printables if c not in '()')
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
        self.variable.addParseAction(self._variable_action)
        self.constant.addParseAction(self._constant_action)
        self.compound_term.addParseAction(self._compound_term_action)
        self.statement.addParseAction(self._statement_action)
        self.statements.addParseAction(self._statements_action)

    @staticmethod
    def _variable_action(toks):
        return Var('_' + toks.variable[0])

    @staticmethod
    def _statement_action(toks):
        return toks.statement

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
    Reads game rules, ignores comments.
    :param gdl_file: path to input file
    :return: string of game rules in KIF format
    """
    with open(gdl_file) as f:
        gdl_rules = '\n'.join(line for line in (line.strip() for line in f.readlines())
                              if line and not line.startswith(';'))
    return gdl_rules


def write_rules(output_file, problogterms):
    """
    Writes a list of problog terms onto a designated output file.
    :param output_file: path to output file
    :param problogterms: list of Problog Term
    :return: None
    """
    with open(output_file, 'w') as f:
        f.write(problogterms2problogstring(problogterms))


def gdlstring2problogterms(gdl_rules):
    """
    Parses into a ProbLog term representing a Prolog list
    :param gdl_rules: a string of gdl_rules in KIF format (Knowledge Interchange Format)
    :return: ProbLog term representing a Prolog list
    """
    return list(GDLtoProbLogParser().statements.parseString(gdl_rules, parseAll=True))


def problogterms2problogstring(problogterms):
    return '\n'.join([str(term) + '.' for term in problogterms])
