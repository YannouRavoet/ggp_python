import argparse

from utils.gdl_parser import read_rules, write_rules, gdl2prolog

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-g', '--game', dest='game', type=str, required=True,
                        help='<game>.gdl file to parse')
    args = parser.parse_args()

    gdl_rules = read_rules(f'../games/{args.game}.gdl')
    prolog_rules = gdl2prolog(gdl_rules)
    write_rules(f'../games/{args.game}.pl', prolog_rules)

    dynamics = read_rules('../src/utils/prolog/dynamics.pl', cmt_token='%')
    ggp = read_rules('../src/utils/prolog/ggp.pl', cmt_token='%')
    ggp_ii = read_rules('../src/utils/prolog/ggp_ii.pl', cmt_token='%')
    ggp_sto = read_rules('../src/utils/prolog/ggp_sto.pl', cmt_token='%')
    ggp_sto_psm = read_rules('../src/utils/prolog/ggp_sto.psm', cmt_token='%')

    prolog_file = '\n'.join([":- style_check(-discontiguous).",
                             ":- style_check(-singleton).",
                             ":- use_module(library(prism)).",
                             ":- prism_start(path(prism)).",
                             ":- load_prism('test_psm.psm').",
                             prolog_rules,
                             dynamics,
                             ggp,
                             ggp_ii,
                             ggp_sto])
    write_rules('../test_pl.pl', prolog_file)

    psm_file = '\n'.join([prolog_rules,
                          dynamics,
                          ggp_sto_psm])
    write_rules('../test_psm.psm', psm_file)
