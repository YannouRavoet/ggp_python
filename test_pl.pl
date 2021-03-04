:- style_check(-singleton), style_check(-discontiguous).
clear_engine :-
setof(B, (base(B), B), StateFacts),
maplist(retractall,StateFacts),
maplist(retractall,does(_,_)).
distinct(X,Y) :-
X \= Y.
gteq(X,Y):- X >= Y.
zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs,Ys,Zs).
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
minmax_goals(Role, Min, Max):-
findall(Goal, clause(goal(Role, Goal), _), Goals),
min_list(Goals, Min),
max_list(Goals, Max).
legal_jointaction(State, JointAction) :-
roles_pl(Roles),
legal_jointaction_iter(State, Roles, [], JointAction).
legal_jointaction_iter(_, [], JointAction, JointAction).
legal_jointaction_iter(State, [RoleH|RoleT], Temp, JointAction) :-
legal_pl(State, RoleH, Action),
legal_jointaction_iter(State, RoleT, [Action|Temp], JointAction).
legal_jointaction_random(JointAction):-
roles_pl(Roles),
legal_jointaction_random_iter(Roles, [], JointAction).
legal_jointaction_random_iter([], JointAction, JointAction).
legal_jointaction_random_iter([RoleH|RoleT], Temp, JointAction) :-
legals_pl(RoleH, Actions),
random_member(Action, Actions),
legal_jointaction_random_iter(RoleT, [Action|Temp], JointAction).
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
simulate(State, Role, Total, Rounds) :-
simulate_iter(State, Role, 0, Total, Rounds).
simulate_iter(_, _, Total, Total, 0) :- !.
simulate_iter(State, Role, Temp, Total, Rounds):-
simulate(State, Role, NewValue),
NewTemp is Temp + NewValue,
NewRounds is Rounds - 1,
simulate_iter(State, Role, NewTemp, Total, NewRounds).
has_random :-
role(random).
has_imperfect_information :-
clause(sees(_,_), _), !.
has_stochastic_actions :-
clause(action_effects(_,_,_,_), _), !.
sees_pl(State, JointAction, Role, Percepts):-
maplist(assertz, State),
maplist(assertz, JointAction),
setof(sees(Role,P), sees(Role,P), Percepts),
maplist(retract, State),
maplist(retract, JointAction),!.
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
generate_jointactions(StatesInH, Action, Percepts, JointActions),
length(JointActions, Length),
(Length > 0 -> % If no jointactions were found that result in the same Percepts, the state was not true.
build_states(StatesInH, JointActions, Temp, NewTemp),
update_valid_states(StatesInT, Action, Percepts, NewTemp, StatesOut, Terminal)
;
update_valid_states(StatesInT, Action, Percepts, Temp, StatesOut, Terminal)
).
generate_jointactions(State, does(Role,Action), Percepts, JointActions):-
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
has_stochastic_actions :-
clause(outcome(_,_,_,_), _), !.
outcome_pl(State, Role, Action, Outcomes):-
maplist(assertz, State),
outcome(Role,Action, Outcome, Prob),
zip(Outcome, Prob, Outcomes),
maplist(retract, State),!.
sample(Role, Action, Outcome):-
outcome(Role, Action, Outcomes, Probs),
choose(Outcomes, Probs, Outcome).
choose(Outcomes, Probs, Outcome):-
random(P),
choose_iter(Outcomes, Probs, 0, P, Outcome).
choose_iter([Outcome|[]], _, _, _, Outcome).
choose_iter([OutcomesH|OutcomesT], [ProbsH|ProbsT], TempProb, P, Outcome):-
NewTempProb is ProbsH + TempProb,
(P =< NewTempProb ->
Outcome = OutcomesH
;
choose_iter(OutcomesT, ProbsT, NewTempProb, P, Outcome)
).
legal_jointaction_sto(State, JointAction, Outcomes):-
legal_jointaction(State, JointAction),
findall([DJA, Prob],jointaction_outcome(State, JointAction, DJA, Prob),Outcomes).
jointaction_outcome(State, JointAction, DeterministicJointAction, Prob):-
jointaction_outcome_iter(State, JointAction, [], DeterministicJointAction, 1, Prob).
jointaction_outcome_iter(_, [], RevDJA, DJA, Prob, Prob):-
reverse(RevDJA, DJA).
jointaction_outcome_iter(State, [does(Role, Action)|JAT], TempDJA, DJA, TempProb, Prob):-
outcome_pl(State, Role, Action, Outcomes),
member([Outcome,OutcomeProb], Outcomes),
NewTempProb is TempProb * OutcomeProb,
jointaction_outcome_iter(State, JAT, [Outcome|TempDJA], DJA, NewTempProb, Prob).
sample_action_outcome(State, Role, Action, Outcome):-
maplist(assertz, State),
sample(Role, Action, Outcome),
maplist(retract, State), !.
sample_jointaction_outcome(JointAction, Outcomes):-
sample_jointaction_outcome_iter(JointAction, [], Outcomes).
sample_jointaction_outcome(State, JointAction, Outcomes):-
maplist(assertz, State),
sample_jointaction_outcome_iter(JointAction, [], Outcomes),
maplist(retract, State), !.
sample_jointaction_outcome_iter([], RevOutcomes, Outcomes):-
reverse(RevOutcomes, Outcomes),!.
sample_jointaction_outcome_iter([does(Role, Action) | ActionsT], TempOutcomes, Outcomes):-
sample(Role, Action, Outcome),
sample_jointaction_outcome_iter(ActionsT, [Outcome | TempOutcomes], Outcomes).
simulate_sto(State, Role, Value) :-
maplist(assertz, State),
(terminal ->
goal(Role, Value),
maplist(retract, State)
;
legal_jointaction_random(JointAction),
sample_jointaction_outcome(JointAction, Outcomes),
next_pl(Outcomes, NextState),
maplist(retract, State),
simulate_sto(NextState, Role, Value)
),!.
simulate_sto(State, Role, Total, Rounds) :-
simulate_sto_iter(State, Role, 0, Total, Rounds).
simulate_sto_iter(_, _, Total, Total, 0):- !.
simulate_sto_iter(State, Role, Temp, Total, Rounds):-
simulate_sto(State, Role, NewValue),
NewTemp is Temp + NewValue,
NewRounds is Rounds - 1,
simulate_sto_iter(State, Role, NewTemp, Total, NewRounds).
:- dynamic does/2,  %input
location/3, step/1, %maze
location/3, step/1, has_sword/0, robot_dead/0,%maze_guarded
location/3, control/1, %tictactoe
location/3, control/1, %connectfour
clear/1, on/2, table_top/1, step/1, %blocks
clear/1, on/2, lifted/1, step/1, %blocks2p
location/3, blockednorth/2, blockedeast/2, step/1, %bomberman
closed/1, car/1, chosen/1, step/1, %montyhall
location/3, tried/3, step/1, %kriegtictactoe
control/1, location/3, step/1, %transit
rolling_for/1, previous_claimed_values/2, has_dice/3,claiming/1, guessing/1, game_over/1, %ii_meier.gdl
turn/1, step/1, location/3, inPool/2, occupied/3,%stratego
control/1, location/3, moved/2, %amazons
die/3, control/1, step/1, throwndie/2. %dicegame
role(white).
role(black).
base(location(_v, _x, _y)).
base(control(_p)).
base(moved(_x, _y)).
init(location(white, 3, 0)).
init(location(white, 6, 0)).
init(location(white, 0, 3)).
init(location(white, 9, 3)).
init(location(black, 0, 6)).
init(location(black, 9, 6)).
init(location(black, 3, 9)).
init(location(black, 6, 9)).
init(control(white)).
legal(_player, noop):-
	role(_player),
	\+control(_player).
outcome(_player, noop, [does(_player, noop)], [1]).
legal(_player, move(_x1, _y1, _x2, _y2)):-
	control(_player),
	location(_player, _x1, _y1),
	queenmove(_x1, _y1, _x2, _y2),
	\+moved(_x, _y).
outcome(_player, move(_x1, _y1, _x2, _y2), [does(_player, move(_x1, _y1, _x2, _y2))], [1]).
legal(_player, shoot(_x2, _y2, _x3, _y3)):-
	control(_player),
	moved(_x2, _y2),
	queenmove(_x2, _y2, _x3, _y3).
outcome(_player, shoot(_x1, _y1, _x2, _y2), [does(_player, shoot(_x1, _y1, _x2, _y2)), does(_player, miss)], [_p_success, _p_miss]):-
	diff(_x1, _x2, _dx),
	diff(_y1, _y2, _dy),
	max(_dx, _dy, _d),
	diff(_d, 10, _chance),
	is(_p_success, /(_chance, 10)),
	is(_p_miss, /(_d, 10)).
diff(_x1, _x1, 0).
diff(_x1, 0, _x1).
diff(_x1, _x2, _d):-
	gt(_x2, _x1),
	diff(_x2, _x1, _d).
diff(_x1, _x2, _d):-
	gt(_x1, _x2),
	succ(_x1m, _x1),
	succ(_x2m, _x2),
	diff(_x1m, _x2m, _d).
queenmove(_x1, _y1, _x2, _y2):-
	direction(_dir),
	directionmove(_dir, _x1, _y1, _x2, _y2).
directionmove(_dir, _x1, _y1, _x2, _y2):-
	directionstep(_dir, _x1, _y1, _x2, _y2),
	emptycell(_x2, _y2).
directionmove(_dir, _x1, _y1, _x3, _y3):-
	directionstep(_dir, _x1, _y1, _x2, _y2),
	emptycell(_x2, _y2),
	directionmove(_dir, _x2, _y2, _x3, _y3).
emptycell(_x, _y):-
	cell(_x, _y),
	\+location(_object, _x, _y).
next(control(_player)):-
	control(_player),
	\+does(_player, shoot(_x1, _y1, _x2, _y2)),
	\+does(_player, miss).
next(control(black)):-
	(does(white, shoot(_x2, _y2, _x3, _y3));does(white, miss)).
next(control(white)):-
	(does(black, shoot(_x2, _y2, _x3, _y3));does(black, miss)).
next(location(arrow, _x3, _y3)):-
	role(_player),
	does(_player, shoot(_x2, _y2, _x3, _y3)).
next(location(_player, _x2, _y2)):-
	role(_player),
	does(_player, move(_x1, _y1, _x2, _y2)).
next(moved(_x2, _y2)):-
	role(_player),
	does(_player, move(_x1, _y1, _x2, _y2)).
next(location(_object, _x, _y)):-
	location(_object, _x, _y),
	\+does(_player, move(_x, _y, _x2, _y2)).
terminal:-
	control(_r),
	\+legalMove(_r).
legalMove(_r):-
	location(_r, _x1, _y1),
	queenmove(_x1, _y1, _x2, _y2).
goal(white, 100):-
	control(black).
goal(black, 100):-
	control(white).
goal(white, 0):-
	control(white).
goal(black, 0):-
	control(black).
directionstep(n, _x, _y1, _x, _y2):-
	index(_x),
	succ(_y1, _y2).
directionstep(s, _x, _y1, _x, _y2):-
	index(_x),
	succ(_y2, _y1).
directionstep(e, _x1, _y, _x2, _y):-
	succ(_x1, _x2),
	index(_y).
directionstep(w, _x1, _y, _x2, _y):-
	succ(_x2, _x1),
	index(_y).
directionstep(ne, _x1, _y1, _x2, _y2):-
	succ(_x1, _x2),
	succ(_y1, _y2).
directionstep(sw, _x1, _y1, _x2, _y2):-
	succ(_x2, _x1),
	succ(_y2, _y1).
directionstep(se, _x1, _y1, _x2, _y2):-
	succ(_x1, _x2),
	succ(_y2, _y1).
directionstep(nw, _x1, _y1, _x2, _y2):-
	succ(_x2, _x1),
	succ(_y1, _y2).
distinctcell(_x1, _y1, _x2, _y2):-
	cell(_x1, _y1),
	cell(_x2, _y2),
	distinct(_x1, _x2).
distinctcell(_x1, _y1, _x2, _y2):-
	cell(_x1, _y1),
	cell(_x2, _y2),
	distinct(_y1, _y2).
cell(_x, _y):-
	index(_x),
	index(_y).
direction(n).
direction(ne).
direction(e).
direction(se).
direction(s).
direction(sw).
direction(w).
direction(nw).
index(0).
index(1).
index(2).
index(3).
index(4).
index(5).
index(6).
index(7).
index(8).
index(9).
gt(_v1, 0):-
	distinct(_v1, 0).
gt(_v1, _v2):-
	succ(_v1min, _v1),
	succ(_v2min, _v2),
	gt(_v1min, _v2min).
equal(_x, _x).
max(_x, _y, _x):-
	gt(_x, _y).
max(_x, _y, _y):-
	gt(_y, _x).
max(_x, _x, _x).