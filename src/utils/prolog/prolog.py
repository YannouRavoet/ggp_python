import tempfile
from pyswip import Prolog
from utils.gdl_parser import gdl2prolog, read_rules


class PrologEngine:
    def __init__(self, gdl_rules):
        self.prolog = Prolog()
        game_rules = gdl2prolog(gdl_rules)                                          # game rules
        dynamics = read_rules('src/utils/prolog/dynamics.pl', cmt_token='%')        # dynamic predicates
        ggp = read_rules('src/utils/prolog/ggp.pl', cmt_token='%')                  # perfect information methods
        ggp_ii = read_rules('src/utils/prolog/ggp_ii.pl', cmt_token='%')            # imperfect information methods
        ggp_sto = read_rules('src/utils/prolog/ggp_sto.pl', cmt_token='%')          # stochastic methods
        ggp_stoii = read_rules('src/utils/prolog/ggp_stoii.pl', cmt_token='%')      # stochastic ii methods
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl') as pl:
            pl.write('\n'.join([ggp, ggp_ii, ggp_sto, ggp_stoii, dynamics, game_rules]))
            pl.seek(0)
            self.prolog.consult(pl.name)

    def query(self, query):
        list(self.prolog.query('clear_engine'))
        results = list(self.prolog.query(query))
        return results

    @staticmethod
    def results2list(results):
        return [PrologEngine._result2string(result) for result in results]

    @staticmethod
    def _result2string(result):
        if isinstance(result, str):
            return result
        return result.value

    @staticmethod
    def string2list(string):
        string = string[1:-1]
        parenthesis = 0
        results = list()
        temp = ""
        for i in range(len(string)):
            if string[i] == ',' and parenthesis == 0:
                results.append(temp)
                temp = ""
                continue
            elif string[i] == "(":
                parenthesis += 1
            elif string[i] == ")":
                parenthesis -= 1
            temp += string[i]
        if temp != "":
            results.append(temp)
        return results
