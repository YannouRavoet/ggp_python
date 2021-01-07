from enum import Enum


class MessageType(Enum):
    """""""""""""""""""""
    GAME MANAGER MESSAGES
    """""""""""""""""""""
    START = 0               # start(<matchid>, <gdl_rules>, <startclock>, <playclock>)
    PLAY = 1                # play(<matchid>, <[actions]>)
    STOP = 2                # stop(<matchid>, <[actions]>)
    PLAY_II = 3             # play(<machtid>, <round>, <own_action>, <[percepts]>)
    STOP_II = 4             # stop(<matchid>, <round>, <own_action>, <[percepts]>)
    PLAY_STO = 5            # play(<matchid>, <round>, <own_action>, <[percepts]>)
    STOP_STO = 6
    """""""""""""""""""""
    GAME PLAYER MESSAGES
    """""""""""""""""""""
    READY = 100             # ready
    ACTION = 101            # <action>
    DONE = 102              # done