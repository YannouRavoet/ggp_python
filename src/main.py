from utils.gdl_parsing import read_rules, parse_gdlrules_to_problogterms


if __name__ == "__main__":
    gdl_rules = read_rules('../games/tictactoe.gdl')
    problog_rules = parse_gdlrules_to_problogterms(gdl_rules)
    problog_str = str(problog_rules)
    with open('../games/tictactoe.pl','w') as f:
        f.write(problog_str)
    print('end')







