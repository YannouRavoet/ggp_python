class Player(object):
    def rcv_msg(self, msg):
        if msg.startswith('START'):
            self.handle_start(msg)
        elif msg.startswith('PLAY'):
            self.handle_play(msg)
        elif msg.startswith('STOP'):
            self.handle_stop(msg)
        else:
            pass

    # (START GAMEID ROLE GAMEDESCRIPTION STARTCLOCK PLAYCLOCK)
    def handle_start(self, msg):
        pass

    # (PLAY GAMEID MOVES)
    def handle_play(self, msg):
        pass

    # (STOP GAMEID MOVES)
    def handle_stop(self, msg):
        pass
