from problog.logic import Term, Constant


class Role(Term):
    def __init__(self, role):
        super().__init__('role', [Constant(role)])
        self.role = self.args[0]

    def __repr__(self):
        super().__repr__()


class Action(Term):
    def __init__(self, role, action):
        super().__init__(functor='does', *[role.role, action])
        self.role = self.args[0]
        self.action = self.args[1]


class Percept(Term):
    def __init__(self, role, percept):
        super().__init__(functor='sees', *[role.role, percept])
        self.role = self.args[0]
        self.percept = self.args[1]
