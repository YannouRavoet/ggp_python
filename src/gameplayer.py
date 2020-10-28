import argparse
from http.server import HTTPServer
from utils.ggp import MatchEntry, Simulator
from utils.msg import MessageType, MessageHandler, Message


# TODO: Create child class for GDL-II that overwrites handle_play and handle_stop.
#       Potentially overwrites handle_start to make a list of self.match['State']
class GamePlayer(HTTPServer):
    def __init__(self, name, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.name = name
        self.match = dict()

    def handle_message(self, msg):
        if msg.type == MessageType.START:
            return self.handle_start(msg.args[0], msg.args[1], msg.args[2], msg.args[3], msg.args[4])
        elif msg.type == MessageType.PLAY or msg.type == MessageType.PLAY_II:
            return self.handle_play(msg.args[0], msg.args[1])
        elif msg.type == MessageType.STOP or msg.type == MessageType.STOP_II:
            return self.handle_stop(msg.args[0], msg.args[1])
        else:
            raise NotImplementedError

    def handle_start(self, matchID, role, gdl_rules, startclock, playclock):
        simulator = Simulator(gdl_rules)
        self.match = {'MatchEntry': MatchEntry(matchID, gdl_rules, startclock, playclock),
                      'Simulator': simulator,
                      'Role': role,
                      'State': simulator.initial_state()}
        return Message(MessageType.READY)

    def handle_play(self, matchID, jointactions):
        if None not in jointactions:
            self.match['State'] = self.match['Simulator'].next_state(self.match['State'], jointactions)
        action = self.calculate_action()
        return Message(MessageType.ACTION, [action])

    def handle_stop(self, matchID, jointactions):
        self.match['State'] = self.match['Simulator'].next_state(self.match['State'], jointactions)
        goal_value = self.match['Simulator'].goal(self.match['State'], self.match['Role'])
        self.match['MatchEntry'].add_result(self.name, self.match['Role'], goal_value)
        return Message(MessageType.DONE)

    def calculate_action(self):
        raise NotImplementedError


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
    from gameplayers import LegalPlayer

    player = LegalPlayer(args.name, args.port)
    try:
        player.serve_forever()
    except KeyboardInterrupt:
        print('Shutting down player...')
        player.socket.close()
