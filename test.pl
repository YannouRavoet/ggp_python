role(robot).
base(location(_o, _x, _y)).
base(step(_s)).
init(location(robot, 0, 4)).
init(location(wall, 0, 0)).
init(location(wall, 1, 1)).
init(location(wall, 1, 3)).
init(location(wall, 1, 4)).
init(location(wall, 2, 4)).
init(location(wall, 3, 2)).
init(location(wall, 3, 1)).
init(location(wall, 4, 3)).
init(location(gold, 4, 2)).
init(step(1)).
next(location(wall, _x, _y)):-
	location(wall, _x, _y).
next(location(gold, _x, _y)):-
	location(gold, _x, _y).
next(location(robot, _x, _yup)):-
	does(robot, move_up),
	location(robot, _x, _y),
	succ(_yup, _y).
next(location(robot, _x, _ydown)):-
	does(robot, move_down),
	location(robot, _x, _y),
	succ(_y, _ydown).
next(location(robot, _xright, _y)):-
	does(robot, move_right),
	location(robot, _x, _y),
	succ(_x, _xright).
next(location(robot, _xleft, _y)):-
	does(robot, move_left),
	location(robot, _x, _y),
	succ(_xleft, _x).
next(step(_s2)):-
	step(_s),
	succ(_s, _s2).
legal(robot, move_up):-
	location(robot, _x, _y),
	>(_y, 0),
	succ(_yup, _y),
	\+location(wall, _x, _yup).
legal(robot, move_down):-
	location(robot, _x, _y),
	<(_y, 4),
	succ(_y, _ydown),
	\+location(wall, _x, _ydown).
legal(robot, move_right):-
	location(robot, _x, _y),
	<(_x, 4),
	succ(_x, _xright),
	\+location(wall, _xright, _y).
legal(robot, move_left):-
	location(robot, _x, _y),
	>(_x, 0),
	succ(_xleft, _x),
	\+location(wall, _xleft, _y).
goal(robot, 100):-
	location(robot, _x, _y),
	location(gold, _x, _y).
goal(robot, 0):-
	location(robot, _x, _y),
	location(gold, _xg, _yg),
	or(distinct(_x, _xg), distinct(_y, _yg)).
terminal:-
	step(20).
terminal:-
	location(robot, _x, _y),
	location(gold, _x, _y).
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                               helper methods                                                       */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
clear_engine:-
setof(B, (base(B), B), StateFacts),
maplist(retractall,StateFacts),
maplist(retractall,does(_,_)).
distinct(X,Y) :-
X \= Y.
or(X,_):-X.
or(_,Y):-Y.
gteq(X,Y):- X >= Y.
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
minmax_goals(Role, Min, Max):-
findall(Goal, clause(goal(Role, Goal), _), Goals),
min_list(Goals, Min),
max_list(Goals, Max).
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                           Game Settings                                                            */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
has_random:-
role(random).
has_imperfect_information:-
clause(sees(_,_), _), !.
has_stochastic_actions:-
fail.
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                  GDL methods                                                       */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
legal_jointaction(State, JointAction) :-
roles_pl(Roles),
legal_jointaction_iter(State, Roles, [], JointAction).
legal_jointaction_iter(_, [], JointAction, JointAction).
legal_jointaction_iter(State, [RoleH|RoleT], Temp, JointAction) :-
legal_pl(State, RoleH, Action),
legal_jointaction_iter(State, RoleT, [Action|Temp], JointAction).
legal_jointaction_random(State, JointAction):-
roles_pl(Roles),
legal_jointaction_random_iter(State, Roles, [], JointAction).
legal_jointaction_random_iter(_, [], JointAction, JointAction).
legal_jointaction_random_iter(State, [RoleH|RoleT], Temp, JointAction) :-
legals_pl(State, RoleH, Actions),
random_member(Action, Actions),
legal_jointaction_iter(State, RoleT, [Action|Temp], JointAction).
simulate(State, Role, Value) :-
(terminal_pl(State) ->
goal_pl(State, Role, Value)
;
legal_jointaction_random(State, JointAction),
next_pl(State, JointAction, NextState),
simulate(NextState, Role, Value)
),!.
simulate(State, Role, Total, Rounds) :-
simulate_iter(State, Role, 0, Total, Rounds).
simulate_iter(_, _, Total, Total, 0) :- !.
simulate_iter(State, Role, Temp, Total, Rounds):-
simulate(State, Role, NewValue),
NewTemp is Temp + NewValue,
NewRounds is Rounds - 1,
simulate_iter(State, Role, NewTemp, Total, NewRounds).
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                               GDL-II methods                                                       */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
update_valid_states(StatesIn, Action, Percepts, StatesOut, Terminal):-
update_valid_states(StatesIn, Action, Percepts, [], StatesOut, Terminal).
update_valid_states([], _, _, TempStates, StatesOut, Terminal):-
list_to_set(TempStates, StatesOutUnfiltered),
(Terminal ->
filter_terminals(StatesOutUnfiltered, StatesOut)
;
filter_nonterminals(StatesOutUnfiltered, StatesOut)
),!.
update_valid_states([StatesInH|StatesInT], Action, Percepts, Temp, StatesOut, Terminal):-
generate_jointactions_ii(StatesInH, Action, Percepts, JointActions),
length(JointActions, Length),
(Length > 0 -> % If no jointactions were found that result in the same Percepts, the state was not true.
build_states(StatesInH, JointActions, Temp, NewTemp),
update_valid_states(StatesInT, Action, Percepts, NewTemp, StatesOut, Terminal)
;
update_valid_states(StatesInT, Action, Percepts, Temp, StatesOut, Terminal)
).
generate_jointactions_ii(State, does(Role,Action), Percepts, JointActions):-
setof(JA, legal_jointaction_complete(State, does(Role,Action), JA), LegalJointActions),
filter_jointactions_ii(State, Percepts, Role, LegalJointActions, [], JointActions).
filter_jointactions_ii(_, _, _, [], JointActions, JointActions):- !.
filter_jointactions_ii(State, Percepts, Role, [LJAH|LJAT], Temp, JointActions):-
sees_pl(State, LJAH, Role, JAPercepts),
(JAPercepts == Percepts ->
filter_jointactions_ii(State, Percepts, Role, LJAT, [LJAH|Temp],  JointActions)
;
filter_jointactions_ii(State, Percepts, Role, LJAT, Temp, JointActions)
).
legal_jointaction_complete(State, does(Role,Action), JointAction):-
setof(R, (role(R), R\=Role), OtherRoles),
legal_jointaction_iter(State, OtherRoles, [does(Role,Action)], JointAction).
build_states(_, [], StatesOut, StatesOut):- !.
build_states(StateIn, [JointActionsH|JointActionsT], Temp, StatesOut):-
next_pl(StateIn, JointActionsH, StateOut),
build_states(StateIn, JointActionsT, [StateOut|Temp], StatesOut).
filter_terminals(StatesIn, StatesOut):-
filter_terminals(StatesIn, [], StatesOut).
filter_terminals([], StatesOut, StatesOut).
filter_terminals([StatesInH|StatesInT], Temp, StatesOut):-
(terminal_pl(StatesInH) ->
filter_terminals(StatesInT, [StatesInH|Temp], StatesOut)
;
filter_terminals(StatesInT, Temp, StatesOut)
).
filter_nonterminals(StatesIn, StatesOut):-
filter_nonterminals(StatesIn, [], StatesOut).
filter_nonterminals([], StatesOut, StatesOut).
filter_nonterminals([StatesInH|StatesInT], Temp, StatesOut):-
(terminal_pl(StatesInH) ->
filter_nonterminals(StatesInT, Temp, StatesOut)
;
filter_nonterminals(StatesInT, [StatesInH|Temp], StatesOut)
).
create_valid_states(ActionHist, PerceptHist, StatesOut, Terminal):-
init_pl(S0),
create_valid_states([S0], ActionHist, PerceptHist, StatesOut, Terminal).
create_valid_states(StatesIn, [Action], [Percepts], StatesOut, Terminal):-
update_valid_states(StatesIn, Action, Percepts, StatesOut, Terminal).
create_valid_states(StatesIn, [ActionHistH|ActionHistT], [PerceptHistH|PerceptHistT], StatesOut, Terminal):-
update_valid_states(StatesIn, ActionHistH, PerceptHistH, NewStatesIn, false),
create_valid_states(NewStatesIn, ActionHistT, PerceptHistT, StatesOut, Terminal).
total_goal(States, Role, Goal):-
total_goal(States, Role, 0, Goal).
total_goal([], _, Goal, Goal).
total_goal([StatesH|StatesT], Role, Temp, Goal):-
goal_pl(StatesH, Role, NewGoal),
NewTemp is Temp + NewGoal,
total_goal(StatesT, Role, NewTemp, Goal).
filter_states_percepts(States, JointActions, Role, Percepts, Indices):-
filter_states_percepts(States, JointActions, Role, Percepts, 0, [], Indices).
filter_states_percepts([], [], _, _, _, Indices, Indices):- !.
filter_states_percepts([StatesH|StatesT], [JointActionsH|JointActionsT], Role, Percepts, CurInd, TempInds, Indices):-
sees_pl(StatesH, JointActionsH, Role, ActualPercepts),
NewInd is CurInd + 1,
(ActualPercepts == Percepts ->
filter_states_percepts(StatesT, JointActionsT, Role, Percepts, NewInd, [CurInd|TempInds], Indices)
;
filter_states_percepts(StatesT, JointActionsT, Role, Percepts, NewInd, TempInds, Indices)
).
avg_goal_from_history(ActionHist, PerceptHist, Role, Goal):-
create_valid_states(ActionHist, PerceptHist, StatesOut, true),
total_goal(StatesOut, Role, GoalSum),
length(StatesOut, L),
Goal is div(GoalSum, L).
simulate_multi(States, Role, Rounds, Values):-
simulate_multi(States, Role, Rounds, [], Values).
simulate_multi([],_,_,TempValues, Values):-
reverse(TempValues, Values).
simulate_multi([StatesH|StatesT],Role,Rounds,TempValues, Values):-
simulate(StatesH, Role, Value, Rounds),
simulate_multi(StatesT, Role, Rounds, [Value|TempValues], Values).
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                           Dynamic Predicates                                                       */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
:- dynamic does/2,  %input
location/3, step/1, %maze
cell/3, control/1, %tictactoe
cell/3, control/1, %connectfour
clear/1, on/2, table_top/1, step/1, %blocks
clear/1, on/2, lifted/1, step/1, %blocks2p
location/3, blockednorth/2, blockedeast/2, step/1, %bomberman
closed/1, car/1, chosen/1, step/1, %montyhall
cell/3, tried/3, step/1, %kriegtictactoe
control/1, location/3, step/1, %transit
rolling_for/1, previous_claimed_values/2, has_dice/3,claiming/1, guessing/1, game_over/1, %meier.gdl
turn/1, step/1, cell/3, inPool/2, occupied/3. %stratego