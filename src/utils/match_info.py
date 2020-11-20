from enum import Enum


class GameType(Enum):
    GDL = 1
    GDL_II = 2


class MatchInfo(object):
    def __init__(self, matchID, gdl_rules, startclock, playclock):
        self.matchID = matchID
        self.gdl_rules = gdl_rules
        self.startclock = startclock
        self.playclock = playclock
        self.results = dict()

    def add_result(self, role, goal):
        self.results[role] = goal
        print(f"goal [{role}]: {goal}")
