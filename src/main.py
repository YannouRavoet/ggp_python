from utils.gdl_parsing import parse_file

if __name__ == "__main__":
    problog_statement = parse_file('../games/tictactoe.gdl')
    print(problog_statement)

