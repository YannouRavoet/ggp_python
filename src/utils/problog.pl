
% distinct/2 simply returns whether or not X and Y are different.
distinct(X,Y) :-
    X \= Y.
or(X,_):-X.
or(_,Y):-Y.

not(\+X, X).
not(X, \+X).

not(_):-fail.
does(_,_):-fail.
cell(_,_,_):-fail.

% tuple_2_list/2 converts a tuple to a list
tuple_2_list(Tuple, List):-
    tuple_2_list(Tuple, [], List).
tuple_2_list((TH, TT), TempList, List):-
    tuple_2_list(TT, [TH|TempList], List),!.
tuple_2_list(TH, TempList, List):-
    reverse([TH|TempList], List).

% minmax_goals\3 returns the minimum and maximum goal values for a role.
% This is used to normalize goal value between 1-100.
minmax_goals(Role, Min, Max):-
    findall(Goal, clause(goal(Role, Goal), _), Goals),
    min_list(Goals, Min),
    max_list(Goals, Max).


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


% legal_jointaction_complete\2 builds a JointAction that completes the KnownActions.
legal_jointaction_complete(State, KnownActions, JointAction):-
    findall(R, (role(R), \+member(does(R, _), KnownActions)), Roles),
    legal_jointaction_iter(State, Roles, KnownActions, JointAction).

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

% valid_state/2 is true if in State given Percepts,
% the facts that can be inferred from Percepts are true and
% actions that are forced to be taken by any role are legal.
valid_state(State, Percepts):-
    facts_from_clauses(Percepts, State, Facts),
    facts_pl(State, Facts), % fails if non-action facts are not true in State
    add_clauses_information(State, Facts, NewState),
    does_pl(NewState, Actions),
    actions_are_legal(State, Actions),!.


actions_are_legal(_, []).
actions_are_legal(State, [does(R,A)|ActionsT]):-
    legal_pl(State, R, does(R,A)),
    actions_are_legal(State, ActionsT).

add_clauses_information(State, Clauses, NewState):-
    facts_from_clauses(Clauses, [], Facts),
    append(Facts, State, NewState).

% facts_from_clauses\3 is used to generate all facts that can be inferred from the given clauses.
facts_from_clauses([], Facts, Facts).
facts_from_clauses([ClauseH|ClauseT], TempFacts, Facts):-
    facts_from_clause(ClauseH, TempFacts, NewFacts),
    append(NewFacts, TempFacts, NewTempFacts),
    facts_from_clauses(ClauseT, NewTempFacts, Facts).

% Negated Fact: not(and) = or(not)
facts_from_clause(\+Head, TempFacts, [Fact]):-
    facts_from_clause(Head, [], NewFacts),                                      % find new facts
    findall(F, (member(F,NewFacts), \+member(F, TempFacts)), NegatableFacts),   % filter known facts
    member(FactToNegate, NegatableFacts),                                       % choose a fact to negate
    not(FactToNegate, Fact).

% Known fact: From previous clauses or from grounded knowledge
facts_from_clause(Head, TempFacts, []):-
    member(Head, TempFacts).
facts_from_clause(Head, _, []):-
    clause(Head, Body),
    Body == true,!.

% Clause Body = Clause
facts_from_clause(Head, TempFacts, Facts):-
    clause(Head, Body),
    Body\=fail,
    tuple_2_list(Body, BodyList),
    facts_from_clauses(BodyList, TempFacts, Facts),!.

% New fact
facts_from_clause(Head, _, [Head]).


% legal_jointaction_from_percepts\3 can be used to get a random legal action from all roles different than Role,
% that is in line with the passed Percepts in the current state.
legal_jointaction_from_percepts(State, Action, Percepts, JointAction):-
    add_clauses_information(State, Percepts, NewState),
    does_pl(NewState, KnownActions),
    list_to_set([Action|KnownActions], Actions),
    legal_jointaction_complete(NewState, Actions, JointAction).


% update validstates
update_valid_states(States, Action, Percepts, NewStates) :-
    update_valid_states(States, Action, Percepts, [], NewStates),!.

update_valid_states([], _, _, TempStates, NewStates):-
    list_to_set(TempStates, NewStates).
update_valid_states([StatesH|StatesT], Action, Percepts, TempStates, NewStates) :-
    (valid_state(StatesH, Percepts) ->
        findall(JA, legal_jointaction_from_percepts(StatesH, Action, Percepts, JA), JointActions),
        build_states(StatesH, JointActions, [], StatesToAdd),
        append(StatesToAdd, TempStates, NewTempStates),
        update_valid_states(StatesT, Action, Percepts, NewTempStates, NewStates)
    ;
        update_valid_states(StatesT, Action, Percepts, TempStates, NewStates)
    ).

build_states(_, [], States, States):- !.
build_states(State0, [JointActionsH|JointActionsT], TempStates, States):-
    next_pl(State0, JointActionsH, NewState),
    build_states(State0, JointActionsT, [NewState|TempStates], States).





%create_valid_state
create_valid_state(Role, ActionHist, PerceptHist, State):-
    init_pl(TempState),
    create_valid_state(Role, ActionHist, PerceptHist, TempState, State).

create_valid_state(_, [],[],State, State).
create_valid_state(Role, [ActionH|ActionT], [PerceptH|PerceptT], TempState, State):-
    legal_jointaction_complete(TempState, [ActionH], JointAction),
    sees_pl(TempState, JointAction, Role, Percepts),
    Percepts = PerceptH,
    next_pl(TempState, JointAction, NextState),
    create_valid_state(Role, ActionT, PerceptT, NextState, State).


%helper functions to findall facts of a certain kind.
assert_pl(\+Term):-
    assertz(not(Term)), !.
assert_pl(Term):-
    assertz(Term),!.
retract_pl(\+Term):-
    retract(not(Term)),!.
retract_pl(Term):-
    retract(Term),!.


roles_pl(Roles):-
    setof(R, role(R), Roles).
init_pl(State):-
    setof(S, init(S), State).
next_pl(State, JointAction, NextState):-
    maplist(assert_pl, State),
    maplist(assert_pl, JointAction),
    setof(S,next(S), NextState),
    maplist(retract_pl, JointAction),
    maplist(retract_pl, State).
legal_pl(State, Role, Action):-
    legals_pl(State, Role, Actions),
    member(Action, Actions).
legals_pl(State, Role, Actions):-
    maplist(assert_pl, State),
    setof(does(Role,A), (legal(Role,A), \+not(does(Role,A))), Actions),
    maplist(retract_pl, State).
sees_pl(State, JointAction, Role, Percepts):-
    maplist(assert_pl, State),
    maplist(assert_pl, JointAction),
    setof(sees(Role,P), sees(Role,P), Percepts),
    maplist(retract_pl, State),
    maplist(retract_pl, JointAction).
terminal_pl(State):-
    maplist(assert_pl,State),
    (terminal->
        maplist(retract_pl,State) ;
        maplist(retract_pl,State),
        fail).
goal_pl(State, Role, Value):-
    maplist(assert_pl,State),
    goal(Role, Value),
    maplist(retract_pl,State).

does_pl(State, Actions):-
    maplist(assert_pl,State),
    findall(does(R,A), does(R,A), Actions),
    maplist(retract_pl, State).
% facts_pl/2 is true whenever any fact that is not an action, is true in the given state.
facts_pl(State, Facts):-
    maplist(assert_pl, State),
    findall(F, (member(F, Facts), F\=does(_,_)), NonActionFacts),
    maplist(call, NonActionFacts),
    maplist(retract_pl, State).

% DCGs
%init --> {findall(S, init(S), S0)}, S0.
%roles --> {findall(R, role(R), Roles)}, Roles.
%goal(S, Role) --> {maplist(assertz, S),
%                   goal(Role, Goal),
%                   maplist(retract, S)}, [Goal].
%next(S0, JointAction) --> { maplist(assertz, S0),
%                            maplist(assertz, JointAction),
%                            findall(S,next(S), S1),
%                            maplist(retract,S0),
%                            maplist(retract, JointAction)}, S1.
%legal(S, Role) --> {maplist(assertz, S),
%                    findall(does(Role,A), legal(Role,A), Actions),
%                    maplist(retract,S)}, Actions.
%sees(S, Role) --> {maplist(assertz, S),
%                    findall(sees(Role,P), sees(Role,P), Percepts),
%                    maplist(retract,S)}, Percepts.
%terminal(S) --> {maplist(assertz,S),
%                 (terminal->
%                    maplist(retract,S)
%                 ;
%                    maplist(retract,S),
%                    fail
%                 )}.