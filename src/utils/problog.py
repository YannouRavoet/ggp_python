from problog.engine import DefaultEngine
from problog.program import PrologString
from problog.logic import Term, Var, And, Not


class ProblogEngine():
    def __init__(self, problog_rules):
        self.engine = DefaultEngine()
        self.db = self.engine.prepare(PrologString(problog_rules))  # problog.program.ClauseDB

    def query(self, query_term):
        """
        Runs a query on the current model of the engine.
        :param query_term: The term to query. The variable of interest should be None
        :type query_term: Term
        :return: Valid solutions for the variable of interest.
        :type return: list(Term)
        """
        results = self.engine.query(self.db, query_term)
        return [result[query_term.args.index(None)] for result in results]

    def query_bool(self, query_term):
        results = self.engine.query(self.db, query_term)
        return len(results) != 0

    def extend(self, prolog_string):
        for statement in prolog_string:
            self.db += statement




def make_environment(term):
    if isinstance(term, Var):
        return {term.functor: None}
    else:
        env = dict()
        for arg in term.args:
            env.update(make_environment(arg))
        return env


