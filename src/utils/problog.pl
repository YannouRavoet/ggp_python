distinct(X,Y) :-
    X \= Y.
% Backend methods to more efficiently use the ProbLog engine when having to make multiple calls.

% legal_jointaction\1 returns a random jointaction (one action per role) that is legal in the current state.
legal_jointaction(JointAction) :-
    findall(Role, role(Role), Roles),
	legal_jointaction_iter(Roles, [], JointAction).

legal_jointaction_iter([], Temp, JointAction):-
    reverse(Temp, JointAction).
legal_jointaction_iter([RoleH|RoleT], Temp, JointAction) :-
    findall(Action, legal(RoleH, Action), Actions),
    random_member(Action, Actions),
    legal_jointaction_iter(RoleT, [Action|Temp], JointAction).

% simulate\2 simulates a random playthrough from the current state until a terminal state is reached.
% It then finds the goal value for the given role in that terminal state.
simulate(Role, Value) :-
    fail.

