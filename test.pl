role(white).
role(black).
role(random).
index(1).
index(2).
index(3).
base(cell(_m,_n,x)) :- index(_m), index(_n).
base(cell(_m,_n,o)) :- index(_m), index(_n).
base(cell(_m,_n,b)) :- index(_m), index(_n).
base(tried(white,_m,_n)) :- index(_m), index(_n).
base(tried(black,_m,_n)) :- index(_m), index(_n).
input(white,mark(_m,_n)) :- index(_m), index(_n).
input(black,mark(_m,_n)) :- index(_m), index(_n).
input(random,tiebreak(x)).
input(random,tiebreak(o)).
init(cell(1,1,b)).
init(cell(1,2,b)).
init(cell(1,3,b)).
init(cell(2,1,b)).
init(cell(2,2,b)).
init(cell(2,3,b)).
init(cell(3,1,b)).
init(cell(3,2,b)).
init(cell(3,3,b)).
legal(white,mark(_m,_n)) :- index(_m), index(_n), \+tried(white,_m,_n).
legal(black,mark(_m,_n)) :- index(_m), index(_n), \+tried(black,_m,_n).
legal(random,tiebreak(x)).
legal(random,tiebreak(o)).
next(tried(_p,_m,_n)) :- does(_p,mark(_m,_n)).
next(tried(_r,_m,_n)) :- tried(_r,_m,_n).
next(cell(_m,_n,x)) :- does(white,mark(_m,_n)), cell(_m,_n,b), does(black,mark(_j,_k)), or(distinct(_m,_j),distinct(_n,_k)).
next(cell(_m,_n,o)) :- does(black,mark(_m,_n)), cell(_m,_n,b), does(white,mark(_j,_k)), or(distinct(_m,_j),distinct(_n,_k)).
next(cell(_m,_n,_w)) :- cell(_m,_n,b), does(black,mark(_m,_n)), does(white,mark(_m,_n)), does(random,tiebreak(_w)).
next(cell(_m,_n,x)) :- cell(_m,_n,x).
next(cell(_m,_n,o)) :- cell(_m,_n,o).
next(cell(_m,_n,b)) :- cell(_m,_n,b), \+marked(_m,_n).
marked(_m,_n) :- does(_r,mark(_m,_n)).
sees(_r,ok) :- does(_r,mark(_m,_n)), cell(_m,_n,b), does(_s,mark(_j,_k)), or(distinct(_m,_j),distinct(_n,_k)).
sees(white,ok) :- does(white,mark(_m,_n)), cell(_m,_n,b), does(random,tiebreak(x)).
sees(black,ok) :- does(black,mark(_m,_n)), cell(_m,_n,b), does(random,tiebreak(o)).
goal(white,100) :- line(x), \+line(o).
goal(white,50) :- line(x), line(o).
goal(white,50) :- \+open, \+line(x), \+line(o).
goal(white,0) :- line(o), \+line(x).
goal(black,100) :- line(o), \+line(x).
goal(black,50) :- line(x), line(o).
goal(black,50) :- \+open, \+line(x), \+line(o).
goal(black,0) :- line(x), \+line(o).
terminal :- line(x).
terminal :- line(o).
terminal :- \+open.
row(_m,_x) :- cell(_m,1,_x), cell(_m,2,_x), cell(_m,3,_x).
column(_n,_x) :- cell(1,_n,_x), cell(2,_n,_x), cell(3,_n,_x).
diagonal(_x) :- cell(1,1,_x), cell(2,2,_x), cell(3,3,_x).
diagonal(_x) :- cell(1,3,_x), cell(2,2,_x), cell(3,1,_x).
line(_x) :- row(_m,_x).
line(_x) :- column(_m,_x).
line(_x) :- diagonal(_x).
open :- cell(_m,_n,b).
tried(_r,_m,_n) :- fail.

% distinct/2 simply returns whether or not X and Y are different.
distinct(X,Y) :-
    X \= Y.
or(X,_):-X.
or(_,Y):-Y.
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

% legal_jointaction_perm\1 is true whenever JointActions is equal to the list of all legal jointactions in the current state.
legal_jointaction_perm(State, JointActions):-
    findall(JA, legal_jointaction(State, JA), JointActions).

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



% valid_state\1 is true whenever the current state is still true under the given percepts.
% Meaning all actions that are known to have been taken, were indeed legal actions.
valid_state(State, Percepts):-
    known_actions_from_percepts(State, Percepts, Actions),
    actions_are_legal(State, Actions),!.

actions_are_legal(_, []).
actions_are_legal(State, [does(R,A)|ActionsT]):-
    legal_pl(State, R, does(R,A)),
    actions_are_legal(State, ActionsT).

known_actions_from_percepts(State, Percepts, Actions):-
    facts_from_percepts(Percepts, Facts),
    append(Facts, State, NewState),
    does_pl(NewState, Actions).


% facts_from_percepts\3 is used to generate all new facts that are inferred from the given percepts.
% these can then be used to determine legal actions from other roles.
facts_from_percepts(Percepts, Facts):-
    facts_from_percepts(Percepts, [], Facts).

facts_from_percepts([], Facts, Facts).
facts_from_percepts([PerceptH|PerceptT], TempFacts, Facts):-
    facts_from_clause(PerceptH, NewFacts),
    append(NewFacts, TempFacts, NewTempFacts),
    facts_from_percepts(PerceptT, NewTempFacts, Facts).

%Known fact
facts_from_clause(Head, []):-
    clause(Head, Body),
    Body == true, !.
%Clause
facts_from_clause(Head, Facts):-
    clause(Head, Body),
    Body \= fail,
    tuple_2_list(Body, BodyList),
    facts_from_clause(BodyList, [], Facts),!.
facts_from_clause([], Facts, Facts).
facts_from_clause([BodyH|BodyT], TempFacts, Facts):-
    facts_from_clause(BodyH, NewFacts),
    (is_list(NewFacts) ->
        append(NewFacts, TempFacts, NewTempFacts)
    ;
        NewTempFacts = [NewFacts|TempFacts]
    ),
    facts_from_clause(BodyT, NewTempFacts, Facts),!.
%New fact
facts_from_clause(Head, Head).

% legal_jointaction_from_percepts\3 can be used to get a random legal action from all roles different than Role,
% that is in line with the passed Percepts in the current state.
legal_jointaction_from_percepts(State, Action, Percepts, JointAction):-
    known_actions_from_percepts(State, Percepts, KnownActions),
    list_to_set([Action|KnownActions], Actions),
    legal_jointaction_complete(State, Actions, JointAction),!.

legal_jointaction_perm_from_percepts(State, Action, Percepts, JointActions):-
    findall(JA, legal_jointaction_from_percepts(State, Action, Percepts, JA), JointActions).


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
roles_pl(Roles):-
    findall(R, role(R), Roles).
init_pl(State):-
    findall(S, init(S), State).
next_pl(State, JointAction, NextState):-
    maplist(assertz, State),
    maplist(assertz, JointAction),
    findall(S,next(S), NextState),
    maplist(retract, State),
    maplist(retract, JointAction).
legal_pl(State, Role, Action):-
    legals_pl(State, Role, Actions),
    member(Action, Actions).
legals_pl(State, Role, Actions):-
    maplist(assertz, State),
    findall(does(Role,A), legal(Role,A), Actions),
    maplist(retract, State).
sees_pl(State, JointAction, Role, Percepts):-
    maplist(assertz, State),
    maplist(assertz, JointAction),
    findall(sees(Role,P), sees(Role,P), Percepts),
    maplist(retract, State),
    maplist(retract, JointAction).
terminal_pl(State):-
    maplist(assertz,State),
    (terminal->
        maplist(retract,State) ;
        maplist(retract,State),
        fail).
goal_pl(State, Role, Value):-
    maplist(assertz,State),
    goal(Role, Value),
    maplist(retract,State).

does_pl(State, Actions):-
    maplist(assertz,State),
    findall(does(R,A), does(R,A), Actions),
    maplist(retract, State).

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