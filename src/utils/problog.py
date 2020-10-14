from problog.engine import DefaultEngine
from problog.program import PrologString


class ProblogEngine():
    def __init__(self, problog_rules):
        self.engine = DefaultEngine()
        self.db = self.engine.prepare(PrologString(problog_rules))

    def query(self, query_term):
        """
        Runs a query on the current model of the engine.
        :param query_term: The term to query. The variable of interest should be None
        :type query_term: Term
        :return: Valid solutions for the variable of interest.
        :type return: list(Term)
        """
        bool_result = not None in query_term.args
        results = self.engine.query(self.db, query_term)
        if not bool_result:
            return [result[query_term.args.index(None)] for result in results]
        else:
            return len(results) != 0

    def extend(self, prolog_string):
        for statement in prolog_string:
            self.db += statement


