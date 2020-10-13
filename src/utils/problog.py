from problog.engine import DefaultEngine
from problog.program import PrologString


class ProblogEngine():
    def __init__(self, problog_rules):
        self.engine = DefaultEngine()
        self.db = self.engine.prepare(PrologString(problog_rules))

    def query(self, query_term):
        """
        Runs a query on the game model of the engine
        :param evidence: evidence to use before performing the query
        :param query_term: (str) the query to run
        :return: a list of Terms
        """
        results = self.engine.query(self.db, query_term)
        if len(query_term.args) > 0:
            return [result[query_term.args.index(None)] for result in results]
        else:
            # when querying a fact we are only looking at whether it is true or not (f.e. terminal)
            return len(results) != 0

    def extend(self, prolog_string):
        for statement in prolog_string:
            self.db += statement


