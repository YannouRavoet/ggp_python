from typing import Dict


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
        self.results: Dict[str, float] = dict()

    def add_result(self, role, goal):
        self.results[role] = goal
        print(f"goal [{role}]: {goal}")

    def get_winner(self):
        "Returns the winning role of the match as a string. If the match was a tie, returns 'tie'."
        def match_tie():
            match_value = list(self.results.values())[0]
            for result in list(self.results.values()):
                if result != match_value:
                    return False
            return True

        if match_tie():
            return 'tie'
        else:
            winner_value = max(list(self.results.values()))
            winner = list(filter(lambda r: self.results[r] == winner_value, self.results))
            return winner[0]

    def reset(self):
        self.results = dict()
