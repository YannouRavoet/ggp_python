role(white).
role(black).
init(cell(1,1,b)).
init(cell(1,2,b)).
init(cell(1,3,b)).
init(cell(2,1,b)).
init(cell(2,2,b)).
init(cell(2,3,b)).
init(cell(3,1,b)).
init(cell(3,2,b)).
init(cell(3,3,b)).
init(step(1)).
legal(_p,mark(_m,_n)) :- role(_p), cell(_m,_n,_c), distinct(_c,_p), \+tried(_p,_m,_n).
doublehit :- does(white,mark(_m,_n)), does(black,mark(_m,_n)).
validmove(_p,_m,_n) :- does(_p,mark(_m,_n)), cell(_m,_n,b), \+doublehit.
marked(_m,_n) :- does(_p,mark(_m,_n)), \+doublehit.
next(cell(_m,_n,_p)) :- validmove(_p,_m,_n).
next(cell(_m,_n,_c)) :- cell(_m,_n,_c), distinct(_c,b).
next(cell(_m,_n,b)) :- cell(_m,_n,b), \+marked(_m,_n).
next(tried(_p,_m,_n)) :- does(_p,mark(_m,_n)), \+validmove(_p,_m,_n).
next(tried(_p,_m1,_n1)) :- tried(_p,_m1,_n1), does(_p,mark(_m2,_n2)), \+validmove(_p,_m2,_n2).
next(step(_n)) :- step(_m), succ(_m,_n).
sees(_p,yougotit(_m,_n)) :- validmove(_p,_m,_n).
sees(_p,mark(_m,_n)) :- does(_p,mark(_m,_n)), \+validmove(_p,_m,_n).
terminal :- line(white).
terminal :- line(black).
terminal :- \+open.
terminal :- step(30).
line(_c) :- cell(_m,1,_c), cell(_m,2,_c), cell(_m,3,_c).
line(_c) :- cell(1,_n,_c), cell(2,_n,_c), cell(3,_n,_c).
line(_c) :- cell(1,1,_c), cell(2,2,_c), cell(3,3,_c).
line(_c) :- cell(1,3,_c), cell(2,2,_c), cell(3,1,_c).
open :- cell(_m1,_n1,b), \+tried(white,_m1,_n1), cell(_m2,_n2,b), \+tried(black,_m2,_n2).
goal(white,100) :- line(white).
goal(white,50) :- \+line(white), \+line(black).
goal(white,0) :- line(black).
goal(black,100) :- line(black).
goal(black,50) :- \+line(white), \+line(black).
goal(black,0) :- line(white).
tried(_r,_m,_n) :- fail.


/* ++++++++++++++++++ */
/* GDL helper methods */
/* ++++++++++++++++++ */
distinct(X,Y) :-
    X \= Y.
or(X,_):-X.
or(_,Y):-Y.

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
sees_pl(State, JointAction, Role, Percepts):-
    maplist(assertz, State),
    maplist(assertz, JointAction),
    setof(sees(Role,P), sees(Role,P), Percepts),
    maplist(retract, State),
    maplist(retract, JointAction),!.
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

% minmax_goals\3 returns the minimum and maximum goal values for a role.
% This is used to normalize goal values between 0-1.
minmax_goals(Role, Min, Max):-
    findall(Goal, clause(goal(Role, Goal), _), Goals),
    min_list(Goals, Min),
    max_list(Goals, Max).


/* +++++++++++ */
/* GDL methods */
/* +++++++++++ */

% legal_jointaction\1 is true whenever JointAction contains one action per role that is legal in the current state.
% e.g. (Tic Tac Toe): legal_jointaction([does(white, mark(1,1)), does(black, noop)])
legal_jointaction(State, JointAction) :-
    roles_pl(Roles),
    legal_jointaction_iter(State, Roles, [], JointAction).

legal_jointaction_iter(_, [], JointActionRev, JointAction) :-
    reverse(JointActionRev, JointAction).
legal_jointaction_iter(State, [RoleH|RoleT], Temp, JointAction) :-
    legal_pl(State, RoleH, Action),
    legal_jointaction_iter(State, RoleT, [Action|Temp], JointAction).

% legal_jointaction_random\1 is used to generate a random JointAction. Usefull for simulations.
legal_jointaction_random(State, JointAction):-
    roles_pl(Roles),
    legal_jointaction_random_iter(State, Roles, [], JointAction).

legal_jointaction_random_iter(_, [], JointAction, JointAction).
legal_jointaction_random_iter(State, [RoleH|RoleT], Temp, JointAction) :-
    legals_pl(State, RoleH, Actions),
    random_member(Action, Actions),
    legal_jointaction_iter(State, RoleT, [Action|Temp], JointAction).


% simulate\3 simulates a random playthrough from the current state until a terminal state is reached.
% It then finds the goal value for the given role in that terminal state.
simulate(State, Role, Value) :-
    (terminal_pl(State) ->
    	goal_pl(State, Role, Value)
    ;
    	legal_jointaction_random(State, JointAction),
    	next_pl(State, JointAction, NextState),
        simulate(NextState, Role, Value)
    ),!.

% simulate\4 simulates multiple random playthroughs from a given state and sums up the total score for the given role.
simulate(State, Role, Total, Rounds) :-
    simulate_iter(State, Role, 0, Total, Rounds).

simulate_iter(_, _, Total, Total, 0) :- !.
simulate_iter(State, Role, Temp, Total, Rounds):-
    simulate(State, Role, NewValue),
    NewTemp is Temp + NewValue,
    NewRounds is Rounds - 1,
    simulate_iter(State, Role, NewTemp, Total, NewRounds).

/* ++++++++++++++ */
/* GDL-II Methods */
/* ++++++++++++++ */

% update_valid_states/4 updates the list of States given the role's Action and Percepts into a list of valid successor states.
update_valid_states(StatesIn, Action, Percepts, StatesOut):-
    update_valid_states(StatesIn, Action, Percepts, [], StatesOut).

update_valid_states([], _, _, TempStates, StatesOut):-
    list_to_set(TempStates, StatesOut), !.
update_valid_states([StatesInH|StatesInT], Action, Percepts, Temp, StatesOut):-
    generate_jointactions_ii(StatesInH, Action, Percepts, JointActions),
    length(JointActions, Length),
    (Length > 0 -> % If no jointactions were found that result in the same Percepts, the state was not true.
        build_states(StatesInH, JointActions, Temp, NewTemp),
        update_valid_states(StatesInT, Action, Percepts, NewTemp, StatesOut)
    ;
        update_valid_states(StatesInT, Action, Percepts, Temp, StatesOut)
    ).

% generate_jointactions_ii/4 generates all jointactions that complete the role's Action and result in the given Percepts.
generate_jointactions_ii(State, does(Role, Action), Percepts, JointActions):-
    setof(JA, legal_jointaction_complete(State, does(Role, Action), JA), LegalJointActions),
    filter_jointactions_ii(State, Percepts, Role, LegalJointActions, [], JointActions).

filter_jointactions_ii(_, _, _, [], JointActions, JointActions):- !.
filter_jointactions_ii(State, Percepts, Role, [LJAH|LJAT], Temp, JointActions):-
    sees_pl(State, LJAH, Role, JAPercepts),
    (JAPercepts == Percepts ->
        filter_jointactions_ii(State, Percepts, Role, LJAT, [LJAH|Temp],  JointActions)
    ;
        filter_jointactions_ii(State, Percepts, Role, LJAT, Temp, JointActions)
    ).

% legal_jointaction_complete/2 builds a JointAction that completes the Action.
legal_jointaction_complete(State, does(Role,Action), JointAction):-
    setof(R, (role(R), R\=Role), OtherRoles),
    legal_jointaction_iter(State, OtherRoles, [does(Role,Action)], JointAction).

% build_states/4 builds all StatesOut that result from applying JointActions to StateIn.
% StatesOut can contain duplicate states.
build_states(_, [], StatesOut, StatesOut):- !.
build_states(StateIn, [JointActionsH|JointActionsT], Temp, StatesOut):-
    next_pl(StateIn, JointActionsH, StateOut),
    build_states(StateIn, JointActionsT, [StateOut|Temp], StatesOut).



