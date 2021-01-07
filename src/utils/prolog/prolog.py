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
        ggp_sto_psm = read_rules('src/utils/prolog/ggp_sto.psm', cmt_token='%')     # stochastic sampling switches
        with tempfile.NamedTemporaryFile(mode='w', suffix='.psm') as psm:
            psm.write('\n'.join([game_rules, dynamics, ggp_sto_psm]))   # needs the game rules to constrain the switches
            psm.seek(0)
            with tempfile.NamedTemporaryFile(mode='w', suffix='.pl') as pl:
                pl.write(":- style_check(-singleton).\n")
                pl.write(":- style_check(-discontiguous).\n")
                pl.write(":- use_module(library(prism)).\n")
                pl.write(f":- prism_start(path(prism), '{psm.name}.log').\n")
                pl.write(f":- load_prism('{psm.name}').\n")
                pl.write('\n'.join([game_rules, dynamics, ggp, ggp_ii, ggp_sto]))
                pl.seek(0)
                self.prolog.consult(pl.name)

    def query(self, query):
        list(self.prolog.query('clear_engine'))
        results = list(self.prolog.query(query))
        return results

    @staticmethod
    def results2string(results):
        return [PrologEngine.result2string(result) for result in results]

    @staticmethod
    def result2string(result):
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
