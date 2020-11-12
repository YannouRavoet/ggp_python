import argparse

from utils.gdl import read_rules, gdlstring2problogterms, write_rules

if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument('-g', '--game', dest='game', type=str, required=True,
                        help='<game>.gdl file to parse')
    args = parser.parse_args()

    gdl_rules = read_rules(f'../games/{args.game}.gdl')
    problog_rules = gdlstring2problogterms(gdl_rules)
    write_rules(f'../games/{args.game}.pl', problog_rules)
