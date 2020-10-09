from utils.gdl_parsing import parse_file

if __name__ == "__main__":
    problog_statement = parse_file('../games/tictactoe.gdl')
    with open('../games/tictactoe.pl', 'w') as f:
        f.write(problog_statement)

