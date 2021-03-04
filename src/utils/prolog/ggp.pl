:- style_check(-singleton), style_check(-discontiguous).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                             Util Methods                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear_engine :-
    setof(B, (base(B), B), StateFacts),
    maplist(retractall,StateFacts),
    maplist(retractall,does(_,_)).

distinct(X,Y) :-
    X \= Y.

gteq(X,Y):- X >= Y.

zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs,Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               Helper Methods                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
roles_pl(Roles):-
    setof(R, role(R), Roles).
init_pl(State):-
    setof(S, init(S), State).
next_pl(State, JointAction, NextState):-
    maplist(assertz, State),
    maplist(assertz, JointAction),
    setof(S,next(S), NextState),
    maplist(retract, JointAction),
    maplist(retract, State),!.
legal_pl(State, Role, Action):-
    legals_pl(State, Role, Actions),
    member(Action, Actions).
legals_pl(State, Role, LegalActions):-
    maplist(assertz, State),
    setof(does(Role,A), legal(Role,A), LegalActions),
    maplist(retract, State),!.
terminal_pl(State):-
    maplist(assertz,State),
    (terminal->
        maplist(retract,State) ;
        maplist(retract,State),
        fail),!.
goal_pl(State, Role, Value):-
    maplist(assertz,State),
    goal(Role, Value),
    maplist(retract,State),!.


next_pl(JointAction, NextState):-
    maplist(assertz, JointAction),
    setof(S,next(S), NextState),
    maplist(retract, JointAction),!.
legal_pl(Role, Action):-
    legals_pl(Role, Actions),
    member(Action, Actions).
legals_pl(Role, LegalActions):-
    setof(does(Role,A), legal(Role,A), LegalActions),!.
goal_pl(Role, Value):-
    goal(Role, Value),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                 GDL Methods                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% minmax_goals/3 returns the minimum and maximum goal values for a role.
% This is used to normalize goal values between 0-1.
minmax_goals(Role, Min, Max):-
    findall(Goal, clause(goal(Role, Goal), _), Goals),
    min_list(Goals, Min),
    max_list(Goals, Max).

% legal_jointaction/2 is true whenever JointAction contains one action per role that is legal in the current state.
% e.g. (Tic Tac Toe): legal_jointaction(State, [does(white,mark(1,1)), does(black,noop)])
legal_jointaction(State, JointAction) :-
    roles_pl(Roles),
    legal_jointaction_iter(State, Roles, [], JointAction).

legal_jointaction_iter(_, [], JointAction, JointAction).
legal_jointaction_iter(State, [RoleH|RoleT], Temp, JointAction) :-
    legal_pl(State, RoleH, Action),
    legal_jointaction_iter(State, RoleT, [Action|Temp], JointAction).

% legal_jointaction_random/1 is used to generate a random JointAction.
% Usefull for simulations. Assumes the State is already asserted.
legal_jointaction_random(JointAction):-
    roles_pl(Roles),
    legal_jointaction_random_iter(Roles, [], JointAction).

legal_jointaction_random_iter([], JointAction, JointAction).
legal_jointaction_random_iter([RoleH|RoleT], Temp, JointAction) :-
    legals_pl(RoleH, Actions),
    random_member(Action, Actions),
    legal_jointaction_random_iter(RoleT, [Action|Temp], JointAction).


% simulate/3 simulates a random playthrough from the current state
% until a terminal state is reached. It then finds the goal value
% for the given role in that terminal state.
simulate(State, Role, Value) :-
    maplist(assertz, State),
    (terminal ->
    	goal_pl(Role, Value),
    	maplist(retract, State)
    ;
    	legal_jointaction_random(JointAction),
        next_pl(JointAction, NextState),
        maplist(retract, State),
        simulate(NextState, Role, Value)
    ),!.

% simulate/4 simulates multiple playthroughs from a given state
% and sums up the total score for the given role.
simulate(State, Role, Total, Rounds) :-
    simulate_iter(State, Role, 0, Total, Rounds).

simulate_iter(_, _, Total, Total, 0) :- !.
simulate_iter(State, Role, Temp, Total, Rounds):-
    simulate(State, Role, NewValue),
    NewTemp is Temp + NewValue,
    NewRounds is Rounds - 1,
    simulate_iter(State, Role, NewTemp, Total, NewRounds).
