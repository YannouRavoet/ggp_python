import argparse
import signal
import time
from http.server import HTTPServer
from utils.ggp import MatchEntry, Simulator, JointAction
from utils.msg import MessageType, MessageHandler, Message


class ClockOverException(Exception):
    """ An exception raised whenever the start or playclock is over """
    pass


class ClockedFunction:
    def __init__(self, clock, function):
        self.clock = int(clock)
        self.function = function

    def clock_over(self, signum, frame):
        raise ClockOverException()

    def __call__(self, *args, **kwargs):
        old = signal.signal(signal.SIGALRM, self.clock_over)
        signal.alarm(self.clock)
        try:
            result = self.function(*args)
        finally:
            signal.signal(signal.SIGALRM, old)
            signal.alarm(0)
        return result


# TODO: Create child class for GDL-II that overwrites handle_play and handle_stop.
class GamePlayer(HTTPServer):
    def __init__(self, name, port):
        HTTPServer.__init__(self, ('', port), MessageHandler)
        self.name = name
        self.match = dict()

        self.msg_time = None        # time at which last message was received
        self.reply_buffer = 1       # nb seconds between ClockOverException and deadline of response
        self.reply_deadline = None  # time at which the manager needs an answer

    """""""""""
     MESSAGING
    """""""""""
    def handle_message(self, msg, rcvtime):
        self.msg_time = rcvtime
        if msg.type == MessageType.START:
            return self.handle_start(msg.args[0], msg.args[1], msg.args[2], msg.args[3], msg.args[4])
        elif msg.type == MessageType.PLAY:
            return self.handle_play(msg.args[0], msg.args[1])
        elif msg.type == MessageType.STOP:
            return self.handle_stop(msg.args[0], msg.args[1])
        elif msg.type == MessageType.PLAY_II:
            return self.handle_play(msg.args[0], msg.args[1], msg.args[2])
        elif msg.type == MessageType.STOP_II:
            return self.handle_stop(msg.args[0], msg.args[1], msg.args[2])
        else:
            raise NotImplementedError

    def handle_start(self, matchID, role, gdl_rules, startclock, playclock):
        self.set_reply_deadline(startclock)
        simulator = Simulator(gdl_rules)
        self.match = {'MatchEntry': MatchEntry(matchID, gdl_rules, startclock, playclock, simulator.get_gametype()),
                      'Simulator': simulator,
                      'Role': role,
                      'States': [simulator.initial_state()]}
        timed_start = ClockedFunction(self.clock_left(), self.start_player)
        timed_start()
        return Message(MessageType.READY)

    def handle_play(self, matchID, actions, *args, **kwargs):
        self.set_reply_deadline(self.match['MatchEntry'].playclock)
        if len(actions) != 0:
            actions = JointAction(self.match['Simulator'].player_roles(), actions)
            self.match['States'] = [self.match['Simulator'].next_state(self.match['States'][0], actions)]

        search_function = ClockedFunction(self.clock_left(), self.play_player)
        action = search_function(actions)
        return Message(MessageType.ACTION, [action])

    def handle_stop(self, matchID, actions, *args, **kwargs):
        self.set_reply_deadline(self.match['MatchEntry'].playclock)
        actions = JointAction(self.match['Simulator'].player_roles(), actions)
        self.match['States'] = [self.match['Simulator'].next_state(self.match['States'][0], actions)]
        goal_value = self.match['Simulator'].goal(self.match['States'][0], self.match['Role'])
        self.match['MatchEntry'].add_result(self.match['Role'], goal_value)
        return Message(MessageType.DONE)

    """""""""""""""""""""""
    PLAYER IMPLEMENTATIONS
    """""""""""""""""""""""
    def start_player(self):
        pass

    def play_player(self, *args, **kwargs):
        raise NotImplementedError

    """""""""
      TIMERS
    """""""""
    def set_reply_deadline(self, clock_time):
        self.reply_deadline = self.msg_time + clock_time - self.reply_buffer

    def clock_left(self):
        return self.reply_deadline - time.time()


class GamePlayerII(GamePlayer):
    def __init__(self, name, port, nb_states=10):
        super().__init__(name, port)
        self.nb_states = nb_states  # the number of hypothetical states to keep track of

    def handle_play(self, matchID, action, percepts=None, *args, **kwargs):
        self.set_reply_deadline(self.match['MatchEntry'].playclock)
        # FOR EACH BELIEF STATE
            # GET POSSIBLE JOINTACTIONS FROM PERCEPTS
            # UPDATE STATES (ADDING HYPOTHETICAL STATES UNTIL self.nb_states IS REACHED
        # MERGE EQUAL BELIEF STATES
        # SEARCH BEST ACTION
        search_function = ClockedFunction(self.clock_left(), self.play_player)
        action = search_function()

    def handle_stop(self, matchID, action, percepts=None, *args, **kwargs):
        self.set_reply_deadline(self.match['MatchEntry'].playclock)
        # GET POSSIBLE ACTIONS FROM PERCEPTS
        # UPDATE POSSIBLE STATES
        # GET POSSIBLE GOAL VALUES
        return None

    def play_player(self, *args, **kwargs):
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
    from GDL_players import LegalPlayer, RandomPlayer, MCTSPlayer

    player = MCTSPlayer(args.name, args.port, expl_bias=2)
    try:
        player.serve_forever()
    except KeyboardInterrupt:
        print('Shutting down player...')
        player.socket.close()
