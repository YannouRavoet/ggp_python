from utils.gdl import read_rules, gdlstring2problogterms, write_rules

if __name__ == "__main__":
    game = "tictactoe"
    gdl_rules = read_rules(f'../games/{game}.gdl')
    problog_rules = gdlstring2problogterms(gdl_rules)
    write_rules(f'../games/{game}.pl', problog_rules)