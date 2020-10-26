from enum import Enum


class MessageType(Enum):
    """""""""""""""""""""
        SETUP MESSAGES
    """""""""""""""""""""
    ADD_PLAYER = 1

    """""""""""""""""""""
      GAMEPLAY MESSAGES
    """""""""""""""""""""
    START = 100
    PLAY = 101
    STOP = 102
    ABORT = 103

class Message:
    def __init__(self, messagetype):
        self.type = messagetype

    def __repr__(self):
        return str(self)

    def __str__(self):
        return self.type + ' message'

