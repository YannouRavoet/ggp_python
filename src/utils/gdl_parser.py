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


class GDLParser:
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


class GDL2ProLogParser(GDLParser):
    def __init__(self):
        super().__init__()
        self.variable.addParseAction(self._variable_action)
        self.constant.addParseAction(self._constant_action)
        self.compound_term.addParseAction(self._compound_term_action)
        self.statement.addParseAction(self._statement_action)
        self.statements.addParseAction(self._statements_action)

    @staticmethod
    def _constant_action(toks):
        return toks.constant

    @staticmethod
    def _variable_action(toks):
        return f"_{toks.variable[0]}"

    @staticmethod
    def _compound_term_action(toks):
        name = toks.compound_term.name
        args = toks.compound_term.arguments

        if name == '<=':
            head = args[0]
            body = ',\n\t'.join(args[1:])
            return f"{head}:-\n\t{body}"
        elif name == 'or':
            return f"({args[0]};{args[1]})"
        elif name == 'not':
            return f"\+{args[0]}"
        elif name == 'true':
            return args[0]
        elif name == 'listof':
            return f"[{', '.join(args)}]"
        return f"{name}({', '.join(args)})"

    @staticmethod
    def _statement_action(toks):
        return f"{toks.statement}."

    @staticmethod
    def _statements_action(toks):
        return toks.statements


def read_rules(file, cmt_token=';'):
    """
    Reads game rules, ignores comments.
    :param file: path to input file
    :return: str of game rules; each separated by \n
    """
    with open(file) as f:
        rules = '\n'.join(line for line in (line.strip() for line in f.readlines())
                          if line and not line.startswith(cmt_token))
    return rules


def write_rules(output_file, rules):
    """
    Writes a list of problog terms onto a designated output file.
    :param output_file: path to output file
    :param rules: str of game rules; each separated by \n
    :return: None
    """
    with open(output_file, 'w') as f:
        f.write(rules)


def gdl2prolog(gdl_rules):
    statements = list(GDL2ProLogParser().statements.parseString(gdl_rules, parseAll=True))
    return '\n'.join(list(filter(lambda s: not s.startswith('succ('), statements)))
