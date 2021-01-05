import tempfile
from pyswip import Prolog
from utils.gdl_parser import gdl2prolog


class PrologEngine:
    def __init__(self, gdl_rules):
        self.prolog = Prolog()
        self.prolog.consult('src/utils/prolog/dynamics.pl')
        self.prolog.consult('src/utils/prolog/ggp.pl')
        game_rules = gdl2prolog(gdl_rules)
        with tempfile.NamedTemporaryFile(mode='w') as tf:
            tf.write(':- style_check(-singleton).\n')
            tf.write(game_rules)
            tf.seek(0)
            self.prolog.consult(tf.name)

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
