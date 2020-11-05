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

legal_jointaction_pl(JointAction) :-
    findall(Role, role(Role), Roles),
    legal_jointaction_iterpl(Roles, [], JointAction).

legal_jointaction_iterpl([], JointAction, JointAction).
legal_jointaction_iterpl([RoleH|RoleT], Temp, JointAction) :-
    findall(Action, legal(RoleH, Action), Actions),
    random_member(Action, Actions),
    legal_jointaction_iterpl(RoleT, [does(RoleH,Action)|Temp], JointAction).


% simulate\2 simulates a random playthrough from the current state until a terminal state is reached.
% It then finds the goal value for the given role in that terminal state.
simulate(State, Role, Value) :-
	maplist(assertz, State),
    (terminal ->
    	goal(Role, Value),
        maplist(retract, State)
    ;
    	legal_jointaction_pl(JointAction),
        maplist(assertz, JointAction),
    	findall(S, next(S),NextState),
        maplist(retract, State),
        maplist(retract, JointAction),
        simulate(NextState, Role, Value)
    ).

simulate(State, Role, AvgValue,Times) :-
    simulate_iter(State, Role, AvgValue, 0, Times, Times).

simulate_iter(_, _, AvgValue, Value, 0, Times):-
    AvgValue is Value/Times.
simulate_iter(State, Role, AvgValue, Total, Left, Times):-
    simulate(State, Role, NewValue),
    NewTotal is Total + NewValue,
    NewLeft is Left - 1,
    simulate_iter(State, Role, AvgValue, NewTotal, NewLeft, Times).