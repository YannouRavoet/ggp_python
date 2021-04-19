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
(setof(does(Role,A), legal(Role,A), LegalActions) *-> true; LegalActions=[]),   %if no legal actions, return empty list instead of failing
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
findall(Goal, clause(goal(Role, Goal), _0), Goals),
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
legal_jointactions(State, JointActions):-
setof(JA, legal_jointaction(State, JA), JointActions).
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
(setof(sees(Role,P), sees(Role,P), Percepts) *-> true; Percepts=  []),
maplist(retract, State),
maplist(retract, JointAction),!.
legal_jointaction_states(States, JAS):-
legal_jointaction_states(States, [], JAS).
legal_jointaction_states([], JAS, JAS).
legal_jointaction_states([StatesH|StatesT], TempJAS, JAS):-
setof(JA, legal_jointaction(StatesH, JA), StateJAS),
ord_union(TempJAS, StateJAS, NewTempJAS),
legal_jointaction_states(StatesT, NewTempJAS, JAS).
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
legal_jointaction_random_iter(State, OtherRoles, [does(Role,Action)], JointAction).
legal_jointaction_random_iter(_, [], JointAction, JointAction).
legal_jointaction_random_iter(State, [RoleH|RoleT], Temp, JointAction) :-
legals_pl(State, RoleH, Actions),
random_member(Action, Actions),
legal_jointaction_random_iter(State, RoleT, [Action|Temp], JointAction).
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
avg_goal(States, Role, AvgGoal):-
total_goal(States, Role, Goal),
length(States, L),
AvgGoal is div(Goal, L).
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
avg_goal_from_history(ActionHist, PerceptHist, Role, AvgGoal):-
create_valid_states(ActionHist, PerceptHist, StatesOut, true),
avg_goal(StatesOut, Role, AvgGoal).
simulate_multi(States, Role, Rounds, Values):-
simulate_multi(States, Role, Rounds, [], Values).
simulate_multi([],_,_,TempValues, Values):-
reverse(TempValues, Values).
simulate_multi([StatesH|StatesT],Role,Rounds,TempValues, Values):-
simulate(StatesH, Role, Value, Rounds),
simulate_multi(StatesT, Role, Rounds, [Value|TempValues], Values).
has_stochastic_actions :-
clause(outcome(_,_,_,_), _), !.
outcome_pl(State, Role, Action, Outcomes, Probs):-
maplist(assertz, State),
outcome(Role,Action, Outcomes, Probs),
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
outcome_pl(State, Role, Action, Outcomes, Probs),
zip(Outcomes, Probs, OutcomeProbMap),
member([Outcome,OutcomeProb], OutcomeProbMap),
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
legal_actions_states(States, Role, LegalActions):-
legal_actions_states(States, Role, [], LegalActions).
legal_actions_states([], _,  LegalActions, LegalActions).
legal_actions_states([StatesH|StatesT], Role,  TempLegalActions, LegalActions):-
legals_pl(StatesH, Role, StateLegalActions),
ord_union(StateLegalActions, TempLegalActions, NewTempLegalActions),
legal_actions_states(StatesT, Role, NewTempLegalActions, LegalActions).
update_states(States, SAction, DAction, Percepts, Terminal, NewStates):-
update_states_iter(States, SAction, DAction, Percepts, Terminal, [], NewStates).
update_states_iter([], _, _, _, _, NewStates, NewStates).
update_states_iter([StatesH|StatesT], SAction, DAction, Percepts, Terminal, TempNewStates, FinalNewStates):-
update_state(StatesH, SAction, DAction, Percepts, Terminal, NewStates),
ord_union(NewStates, TempNewStates, NewTempNewStates),
update_states_iter(StatesT, SAction, DAction, Percepts, Terminal, NewTempNewStates, FinalNewStates).
update_state(State, does(Role, SAction), DAction, Percepts, Terminal, NewStates):-
(setof(SJA, legal_jointaction(State, SJA), SJAS) *-> true; SJAS = []), %All possible stochastic actions
(setof(SJA, (member(SJA, SJAS), member(does(Role, SAction), SJA)), SJAS_Filtered) *-> true; SJAS_Filtered = []), %filtered for the ones that have the right action choice for the player role
(setof(DJA, SJA^(member(SJA, SJAS_Filtered), sja_to_dja(State, SJA, DJA)), DJAS) *-> true; DJAS = []), %all possible outcomes
(setof(DJA, (member(DJA, DJAS), member(DAction, DJA)), DJAS_Filtered0) *-> true; DJAS_Filtered0 = []), %filtered for the correct outcome of the player
(setof(DJA, (member(DJA, DJAS_Filtered0), sees_pl(State, DJA, Role, PerceptsState), PerceptsState = Percepts), DJAS_Filtered1) *-> true; DJAS_Filtered1 = []), %filtered for jointactions that deliver the right percepts
(setof(NextState, DJA^(member(DJA, DJAS_Filtered1), next_pl(State, DJA, NextState)), NextStates) *-> true; NextStates = []),
(Terminal ->
(setof(NextState, (member(NextState, NextStates), terminal_pl(NextState)), NewStates) *-> true; NewStates = [])
;
(setof(NextState, (member(NextState, NextStates), \+terminal_pl(NextState)), NewStates) *-> true; NewStates = [])
).
sja_to_dja(State, SJA, DJA):-
sja_to_dja_iter(State, SJA, [], DJA).
sja_to_dja_iter(_, [], DJA, DJA).
sja_to_dja_iter(State, [does(Role, SAction)|SJAT], TempDJA, DJA):-
outcome_pl(State, Role, SAction, Outcomes, _),
member(DA, Outcomes),
sja_to_dja_iter(State, SJAT, [DA|TempDJA], DJA).
:- dynamic does/2,  %input
loc/3, step/1, %maze
loc/3, step/1, has_sword/0, robot_dead/0,%maze_guarded
loc/3, control/1, %tictactoe
loc/3, control/1, %connectfour
clear/1, on/2, table_top/1, step/1, %blocks
clear/1, on/2, lifted/1, step/1, %blocks2p
location/3, blockednorth/2, blockedeast/2, step/1, %bomberman
closed/1, car/1, chosen/1, step/1, %montyhall
location/3, tried/3, step/1, %kriegtictactoe
control/1, location/3, step/1, %transit
rolling_for/1, previous_claimed_values/2, has_dice/3,claiming/1, guessing/1, game_over/1, %ii_meier.gdl
turn/1, step/1, location/3, inPool/2, occupied/3,%stratego
control/1, location/3, moved/2, %amazons
die/3, control/1, step/1, throwndie/2, %dicegame
loc/3, step/1, control/1, %kttt_sto
asked/0, exploded/0, step/1, armed/1. %explodingbomb
role(white).
role(black).
base(loc(_v, _x, _y)).
base(control(_p)).
base(step(_s)).
init(loc(e, 0, 0)).
init(loc(e, 0, 1)).
init(loc(e, 0, 2)).
init(loc(e, 1, 0)).
init(loc(e, 1, 1)).
init(loc(e, 1, 2)).
init(loc(e, 2, 0)).
init(loc(e, 2, 1)).
init(loc(e, 2, 2)).
init(step(1)).
init(control(white)).
legal(_p, mark(_x, _y)):-
	control(_p),
	loc(_c, _x, _y),
	distinct(_c, _p).
legal(_p, noop):-
	\+control(_p).
validmove(_p, _x, _y):-
	loc(e, _x, _y),
	does(_p, mark(_x, _y)).
outcome(_role, noop, [does(_role, noop)], [1]).
outcome(_role, mark(_x, _y), [does(_role, mark(_x, _y))], [1]):-
	neighbours(_x, _y, _x1, _y1, _x2, _y2, _x3, _y3, _x4, _y4),
	\+legal(_role, mark(_x1, _y1)),
	\+legal(_role, mark(_x2, _y2)),
	\+legal(_role, mark(_x3, _y3)),
	\+legal(_role, mark(_x4, _y4)).
outcome(_role, mark(_x, _y), [does(_role, mark(_x, _y)), does(_role, mark(_x1, _y1))], [0.9, 0.1]):-
	neighbours(_x, _y, _x1, _y1, _x2, _y2, _x3, _y3, _x4, _y4),
	legal(_role, mark(_x1, _y1)),
	\+legal(_role, mark(_x2, _y2)),
	\+legal(_role, mark(_x3, _y3)),
	\+legal(_role, mark(_x4, _y4)).
outcome(_role, mark(_x, _y), [does(_role, mark(_x, _y)), does(_role, mark(_x1, _y1)), does(_role, mark(_x2, _y2))], [0.8, 0.1, 0.1]):-
	neighbours(_x, _y, _x1, _y1, _x2, _y2, _x3, _y3, _x4, _y4),
	legal(_role, mark(_x1, _y1)),
	legal(_role, mark(_x2, _y2)),
	\+legal(_role, mark(_x3, _y3)),
	\+legal(_role, mark(_x4, _y4)).
outcome(_role, mark(_x, _y), [does(_role, mark(_x, _y)), does(_role, mark(_x1, _y1)), does(_role, mark(_x2, _y2)), does(_role, mark(_x3, _y3))], [0.7, 0.1, 0.1, 0.1]):-
	neighbours(_x, _y, _x1, _y1, _x2, _y2, _x3, _y3, _x4, _y4),
	legal(_role, mark(_x1, _y1)),
	legal(_role, mark(_x2, _y2)),
	legal(_role, mark(_x3, _y3)),
	\+legal(_role, mark(_x4, _y4)).
outcome(_role, mark(_x, _y), [does(_role, mark(_x, _y)), does(_role, mark(_x1, _y1)), does(_role, mark(_x2, _y2)), does(_role, mark(_x3, _y3)), does(_role, mark(_x4, _y4))], [0.6, 0.1, 0.1, 0.1, 0.1]):-
	neighbours(_x, _y, _x1, _y1, _x2, _y2, _x3, _y3, _x4, _y4),
	legal(_role, mark(_x1, _y1)),
	legal(_role, mark(_x2, _y2)),
	legal(_role, mark(_x3, _y3)),
	legal(_role, mark(_x4, _y4)).
adjacent(_i, _i1):-
	(--(_i, _i1);++(_i, _i1)).
adjacent(_x, _y, _x1, _y):-
	adjacent(_x, _x1).
adjacent(_x, _y, _x, _y1):-
	adjacent(_y, _y1).
neighbours(_x, _y, _x1, _y1, _x2, _y2, _x3, _y3, _x4, _y4):-
	adjacent(_x, _y, _x1, _y1),
	adjacent(_x, _y, _x2, _y2),
	adjacent(_x, _y, _x3, _y3),
	adjacent(_x, _y, _x4, _y4),
	distinct_cells(_x, _y, _x1, _y1, _x2, _y2, _x3, _y3, _x4, _y4).
distinct_cells(_x, _y, _x1, _y1, _x2, _y2, _x3, _y3, _x4, _y4):-
	distinct_cell(_x, _y, _x1, _y1),
	distinct_cell(_x, _y, _x2, _y2),
	distinct_cell(_x, _y, _x3, _y3),
	distinct_cell(_x, _y, _x4, _y4),
	distinct_cell(_x1, _y1, _x2, _y2),
	distinct_cell(_x1, _y1, _x3, _y3),
	distinct_cell(_x1, _y1, _x4, _y4),
	distinct_cell(_x2, _y2, _x3, _y3),
	distinct_cell(_x2, _y2, _x4, _y4),
	distinct_cell(_x3, _y3, _x4, _y4).
distinct_cell(_x, _y, _x1, _y1):-
	(distinct(_x, _x1);distinct(_y, _y1)).
next(loc(_p, _x, _y)):-
	validmove(_p, _x, _y).
next(loc(_p, _x, _y)):-
	loc(_p, _x, _y),
	distinct(_p, e).
next(loc(e, _x, _y)):-
	loc(e, _x, _y),
	\+validmove(_p, _x, _y).
next(step(_y)):-
	step(_x),
	succ(_x, _y).
next(control(white)):-
	control(black).
next(control(black)):-
	control(white).
sees(_p, valid):-
	validmove(_p, _x, _y).
sees(_p, invalid):-
	does(_p, mark(_x, _y)),
	\+validmove(_p, _x, _y).
sees(_p, nothing):-
	does(_p, noop).
terminal:-
	line(white).
terminal:-
	line(black).
terminal:-
	\+open.
line(_c):-
	loc(_c, _x, 0),
	loc(_c, _x, 1),
	loc(_c, _x, 2).
line(_c):-
	loc(_c, 0, _y),
	loc(_c, 1, _y),
	loc(_c, 2, _y).
line(_c):-
	loc(_c, 0, 0),
	loc(_c, 1, 1),
	loc(_c, 2, 2).
line(_c):-
	loc(_c, 0, 2),
	loc(_c, 1, 1),
	loc(_c, 2, 0).
open:-
	loc(e, _x, _y).
goal(white, 100):-
	line(white).
goal(white, 50):-
	\+line(white),
	\+line(black).
goal(white, 0):-
	line(black).
goal(black, 100):-
	line(black).
goal(black, 50):-
	\+line(white),
	\+line(black).
goal(black, 0):-
	line(white).
equal(_x, _x).
++(-1, 0).
++(0, 1).
++(1, 2).
++(2, 3).
--(3, 2).
--(2, 1).
--(1, 0).
--(0, -1).