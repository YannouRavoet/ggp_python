from utils.gdl import read_rules, gdlstring2problogterms, write_rules


if __name__ == "__main__":
    gdl_rules = read_rules('../games/tictactoe.gdl')
    problog_rules = gdlstring2problogterms(gdl_rules)
    write_rules('../games/tictactoe.pl', problog_rules)
    print('end')







