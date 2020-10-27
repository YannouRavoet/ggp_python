import argparse
from http.server import HTTPServer

from utils.ggp import MatchEntry, Simulator
from utils.msg import MessageType, MessageHandler, Message


class GamePlayer(HTTPServer):
    def __init__(self, name, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.name = name
        self.match = dict()

    def handle_message(self, msg):
        if msg.type == MessageType.START:
            return self.handle_start(msg.args[0], msg.args[1], msg.args[2], msg.args[3], msg.args[4])
        elif msg.type == MessageType.PLAY:
            pass
        elif msg.type == MessageType.STOP:
            pass
        else:
            raise NotImplementedError

    def handle_start(self, matchID, role, gdl_rules, startclock, playclock):
        simulator = Simulator(gdl_rules)
        self.match = {'MatchEntry': MatchEntry(matchID, gdl_rules, startclock, playclock),
                      'Simulator': simulator,
                      'Role': role,
                      'State': simulator.initial_state()}
        return Message(MessageType.READY)

    def handle_play(self, matchID, lastmoves):
        action = None
        #TODO: calculate action
        return Message(MessageType.ACTION, [action])

    def handle_stop(self, matchID, lastmoves):
        # TODO: calculate and save goal values
        return Message(MessageType.DONE)

if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    DEFAULT_NAME = 'Unnamed Game Player'
    DEFAULT_PORT = 5601

    parser = argparse.ArgumentParser()
    parser.add_argument('-n', '--name', dest='name', type=str, default=DEFAULT_NAME,
                        help='name of the game player')
    parser.add_argument('-p', '--port', dest='port', type=int, default=DEFAULT_PORT,
                        help='port to listen to')
    args = parser.parse_args()

    """""""""""""""
    RUN GAMEPLAYER
    """""""""""""""
    player = GamePlayer(args.name, args.port)
    try:
        player.serve_forever()
    except KeyboardInterrupt:
        print('Shutting down player...')
        player.socket.close()
