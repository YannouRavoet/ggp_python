% distinct/2 simply returns whether or not X and Y are different.
distinct(X,Y) :-
    X \= Y.

% minmax_goals\3 returns the minimum and maximum goal values for a role.
% This is used to normalize goal value between 1-100.
minmax_goals(Role, Min, Max):-
    findall(Goal, clause(goal(Role, Goal), _), Goals),
    min_list(Goals, Min),
    max_list(Goals, Max).


% legal_jointaction\1 is true whenever JointAction contains one action per role that is legal in the current state.
% e.g. (Tic Tac Toe): legal_jointaction([does(white, mark(1,1)), does(black, noop)])
legal_jointaction(JointAction) :-
    findall(Role, role(Role), Roles),
    legal_jointaction_iter(Roles, [], JointAction).

legal_jointaction_iter([], JointAction, JointAction).
legal_jointaction_iter([RoleH|RoleT], Temp, JointAction) :-
    findall(Action, legal(RoleH, Action), Actions),
    member(Action, Actions),
    legal_jointaction_iter(RoleT, [does(RoleH,Action)|Temp], JointAction).

% legal_jointaction_random\1 is used to generate a random JointAction. Usefull for simulations
legal_jointaction_random(JointAction):-
    findall(Role, role(Role), Roles),
    legal_jointaction_random_iter(Roles, [], JointAction).

legal_jointaction_random_iter([], JointAction, JointAction).
legal_jointaction_random_iter([RoleH|RoleT], Temp, JointAction) :-
    findall(Action, legal(RoleH, Action), Actions),
    random_member(Action, Actions),
    legal_jointaction_iter(RoleT, [does(RoleH,Action)|Temp], JointAction).

% legal_jointaction_perm\1 is true whenever JointActions is equal to the list of all legal jointactions in the current state.
legal_jointaction_perm(JointActions):-
    findall(JA, legal_jointaction(JA), JointActions).


% simulate\3 simulates a random playthrough from the current state until a terminal state is reached.
% It then finds the goal value for the given role in that terminal state.
simulate(State, Role, Value) :-
	maplist(assertz, State),
    (terminal ->
    	goal(Role, Value),
        maplist(retract, State)
    ;
    	legal_jointaction_random(JointAction),
        maplist(assertz, JointAction),
    	findall(S, next(S),NextState),
        maplist(retract, State),
        maplist(retract, JointAction),
        simulate(NextState, Role, Value)
    ).

% simulate\4 simulates multiple random playthroughs from a given state and sums up the total score for the given role.
simulate(State, Role, Total, Rounds) :-
    simulate_iter(State, Role, 0, Total, Rounds).

simulate_iter(_, _, Total, Total, 0).
simulate_iter(State, Role, Temp, Total, Rounds):-
    simulate(State, Role, NewValue),
    NewTemp is Temp + NewValue,
    NewRounds is Rounds - 1,
    simulate_iter(State, Role, NewTemp, Total, NewRounds).


% facts_from_percepts\3 is used to generate all new facts that are inferred from the given percepts.
% these can then be used to determine legal actions from other roles.
facts_from_percepts(Role, Percepts, Facts):-
    facts_from_percepts(Role, Percepts, [], Facts).

facts_from_percepts(_, [], Facts, Facts).
facts_from_percepts(Role, [PerceptH|PerceptT], TempFacts, Facts):-
    facts_from_clause(sees(candidate, PerceptH), NewFacts),
    append(NewFacts, TempFacts, NewTempFacts),
    facts_from_percepts(Role, PerceptT, NewTempFacts, Facts).

facts_from_clause(Head, []):-
    clause(Head, Body),
    Body == true, !.
facts_from_clause(Head, Facts):-
	clause(Head, Body),
	facts_from_clause(Body, [], Facts),!.
facts_from_clause(Head, Head).

facts_from_clause((BodyH, BodyT), TempFacts, Facts):-
    facts_from_clause(BodyH, NewFacts),
    (is_list(NewFacts) ->
    	append(NewFacts, TempFacts, NewTempFacts)
    ;
    	NewTempFacts = [NewFacts|TempFacts]
    ),
    facts_from_clause(BodyT, NewTempFacts, Facts),!.

facts_from_clause(Body, TempFacts, Facts):-
    facts_from_clause(Body, NewFacts),
    (is_list(NewFacts) ->
    	append(NewFacts, TempFacts, Facts)
    ;
    	Facts = [NewFacts|TempFacts]
    ).

% legal_jointaction_from_percepts\3 can be used to get a random legal action from all roles different than Role,
% that is in line with the passed Percepts.
legal_jointaction_from_percepts(Role, Percepts, JointAction):-
    facts_from_percepts(Role, Percepts, Facts),
    maplist(assertz, Facts),
    findall(R, (role(R), R\=candidate), Roles),
    legal_jointaction_random_iter(Roles, [], JointAction).

legal_jointaction_perm_from_percepts(Role, Percepts, JointActions):-
    facts_from_percepts(Role, Percepts, Facts),
    maplist(assertz, Facts),
    findall(R, (role(R), R\=candidate), Roles),
    findall(JA, legal_jointaction_iter(Roles, [], JA), JointActions).
