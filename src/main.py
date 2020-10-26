from utils.gdl_parser import read_rules, parse_gdlrules_to_problogterms, write_rules


if __name__ == "__main__":
    gdl_rules = read_rules('../games/tictactoe.gdl')
    problog_rules = parse_gdlrules_to_problogterms(gdl_rules)
    write_rules('../games/tictactoe.pl', problog_rules)
    print('end')







