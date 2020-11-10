role(candidate).
role(random).
init(closed(1)).
init(closed(2)).
init(closed(3)).
init(step(1)).
legal(random,hide_car(_d)) :- step(1), closed(_d).
legal(random,open_door(_d)) :- step(2), closed(_d), \+car(_d), \+chosen(_d).
legal(random,noop) :- step(3).
legal(candidate,choose(_d)) :- step(1), closed(_d).
legal(candidate,noop) :- step(2).
legal(candidate,switch) :- step(3).
legal(candidate,noop) :- step(3).
sees(candidate,does(candidate,_m)) :- does(candidate,_m).
sees(candidate,open_door(_d)) :- does(random,open_door(_d)).
sees(candidate,car(_d)) :- step(3), car(_d), next_chosen(_d).
next(car(_d)) :- does(random,hide_car(_d)).
next(car(_d)) :- car(_d).
next(closed(_d)) :- closed(_d), \+does(random,open_door(_d)).
next(chosen(_d)) :- next_chosen(_d).
next_chosen(_d) :- does(candidate,choose(_d)).
next_chosen(_d) :- chosen(_d), \+does(candidate,switch).
next_chosen(_d) :- does(candidate,switch), closed(_d), \+chosen(_d).
next(step(2)) :- step(1).
next(step(3)) :- step(2).
next(step(4)) :- step(3).
terminal :- step(4).
goal(random,100).
goal(candidate,100) :- chosen(_d), car(_d).
goal(candidate,0) :- chosen(_d), \+car(_d).
car(_d) :- fail.
chosen(_d) :- fail.


distinct(X,Y) :-
    X \= Y.
% Backend methods to more efficiently use the ProbLog engine when having to make multiple calls.

/*
make_jointaction(Role, Action, (Role, Action)).
legal_actions(Role, Actions) :-
    findall(Action, legal(Role, Action), Actions).

permutation(Roles, ActionLists, Permutation):-
    permutation_iter(Roles, ActionLists, [], Permutation).

permutation_iter([], [],  Permutations, Permutations).
permutation_iter([RolesH|RolesT], [ActionsH|ActionsT],  TempPermActions, Permutations):-
    member(Action, ActionsH),
    make_jointaction(RolesH, Action, NewPermAction),
    permutation_iter(RolesT, ActionsT, [NewPermAction|TempPermActions], Permutations).

legal_jointaction_permutation(Permutations):-
    findall(Role, role(Role), Roles),
    maplist(legal_actions, Roles, ActionsLists),
    findall(Perm, permutation(Roles, ActionsLists, Perm), Permutations).
*/


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

simulate(State, Role, Total, Rounds) :-
    simulate_iter(State, Role, 0, Total, Rounds).

simulate_iter(_, _, Total, Total, 0).
simulate_iter(State, Role, Temp, Total, Rounds):-
    simulate(State, Role, NewValue),
    NewTemp is Temp + NewValue,
    NewRounds is Rounds - 1,
    simulate_iter(State, Role, NewTemp, Total, NewRounds).


% minmax_goals\3 returns the minimum and maximum goal values for a role.
% This is used to normalize goal value between 1-100.
minmax_goals(Role, Min, Max):-
    findall(Goal, clause(goal(Role, Goal), _), Goals),
    min_list(Goals, Min),
    max_list(Goals, Max).