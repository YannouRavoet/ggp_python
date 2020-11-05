from problog.engine import DefaultEngine
from problog.program import PrologString
from problog.logic import term2list, Var
from utils.gdl import gdlstring2problogstring, read_rules, problogterms2problogstring


class ProblogEngine:
    def __init__(self, gdl_rules):
        self.engine = DefaultEngine()
        self.base_string = '\n'.join([gdlstring2problogstring(gdl_rules), read_rules('utils/problog.pl', cmt_token='%')])
        self.base_db = self.engine.prepare(PrologString(self.base_string))

    def query(self, query, state=None, return_bool=False, backend=None):
        if state is not None:
            state_db = self._create_db(state)
            results = self.engine.query(state_db, query, backend=backend)
        else:
            results = self.engine.query(self.base_db, query, backend=backend)
        return self._return_results(query, results, return_bool, backend=backend)

    def _create_db(self, state):
        state_string = '\n'.join([self.base_string, problogterms2problogstring(state.facts)])
        return self.engine.prepare(PrologString(state_string))

    @staticmethod
    def _return_results(query, results, return_bool, backend):
        if return_bool:
            return len(results) != 0
        else:
            if backend is None:
                target_index = query.args.index(None)
            else:   # swipl-backend
                target_index = query.args.index(list(filter(lambda arg: isinstance(arg, Var), query.args))[0])
            return [result[target_index] for result in results]

    def clear_stack(self):
        self.engine.shrink_stack()
