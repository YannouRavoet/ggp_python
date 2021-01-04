class GameSettings:
    def __init__(self, random=False, imperfect_information=False, stochastic_actions=False):
        self.has_random = random
        self.has_imperfect_information = imperfect_information
        self.has_stochastic_actions = stochastic_actions


class MatchInfo(object):
    def __init__(self, matchID, gdl_rules, startclock, playclock, game_settings):
        self.matchID: str = matchID
        self.gdl_rules: str = gdl_rules
        self.startclock: int = startclock
        self.playclock: int = playclock
        self.settings: GameSettings = game_settings
        self.results: dict() = dict()

    def add_result(self, role, goal):
        self.results[role] = goal
        print(f"goal [{role}]: {goal}")
