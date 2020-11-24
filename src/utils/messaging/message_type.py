from enum import Enum


class MessageType(Enum):
    """""""""""""""""""""
    GAME MANAGER MESSAGES
    """""""""""""""""""""
    START = 0
    PLAY = 1
    STOP = 2
    PLAY_II = 3
    STOP_II = 4
    """""""""""""""""""""
    GAME PLAYER MESSAGES
    """""""""""""""""""""
    READY = 100
    ACTION = 101
    DONE = 102