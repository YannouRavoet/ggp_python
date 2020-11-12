import signal


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