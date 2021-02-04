from enum import Enum


class MessageType(Enum):
    """""""""""""""""""""
    GAME MANAGER MESSAGES
    """""""""""""""""""""
    START = 0               # start(<matchid>, <role>, <gdl_rules>, <startclock>, <playclock>)
    PLAY = 1                # play(<matchid>, <jointaction>)
    STOP = 2                # stop(<matchid>, <jointaction>)
    PLAY_II = 3             # play(<machtid>, <round>, <own_action>, <[percepts]>)
    STOP_II = 4             # stop(<matchid>, <round>, <own_action>, <[percepts]>)
    PLAY_STO = 5            # play(<matchid>, <jointaction>, <jointeffect>)
    STOP_STO = 6            # stop(<matchid>, <jointaction>, <jointeffect>)
    PLAY_STO_II = 7         # play(<matchid>, <round>, <own_action>, <own_effects>, <[percepts]>)
    STOP_STO_II = 8         # stop(<matchid>, <round>, <own_action>, <own_effects>, <[percepts]>)
    """""""""""""""""""""
    GAME PLAYER MESSAGES
    """""""""""""""""""""
    READY = 100             # ready
    ACTION = 101            # <action>
    DONE = 102              # done