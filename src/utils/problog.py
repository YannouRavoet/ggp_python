from copy import deepcopy
from problog.engine import DefaultEngine
from problog.program import PrologString
from utils.gdl import gdlstring2problogterms, problogterms2problogstring


class ProblogEngine():
    def __init__(self, gdl_rules):
        self.engine = DefaultEngine()
        self.problog_terms = gdlstring2problogterms(gdl_rules)
        problog_string = problogterms2problogstring(self.problog_terms)
        self.base_db = self.engine.prepare(PrologString(problog_string))

    def query(self, query, state=None, return_bool=False):
        if state is not None:
            state_db = self._create_db(state)
            results = self.engine.query(state_db, query)
        else:
            results = self.engine.query(self.base_db, query)
        return self._return_results(query, results, return_bool)

    def _create_db(self, state):
        state_terms = deepcopy(self.problog_terms)
        state_terms.extend(state.facts)
        problog_string = problogterms2problogstring(state_terms)
        return self.engine.prepare(PrologString(problog_string))

    @staticmethod
    def _return_results(query, results, return_bool):
        if return_bool:
            return len(results) != 0
        else:
            return [result[query.args.index(None)] for result in results]

    def clear_stack(self):
        self.engine.shrink_stack()
