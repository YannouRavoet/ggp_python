role(random).
role(p1).
role(p2).
init(rolling_for(p1)).
init(previous_claimed_values(0,0)).
legal(random,roll(_p,_x,_y)) :- rolling_for(_p), dots(_x), dots(_y).
legal(_p,noop) :- role(_p), distinct(_p,random), rolling_for(_q).
legal(_p,claim(_x,_y)) :- claiming(_p), previous_claimed_values(_mx,_my), better_values(_mx,_my,_x,_y).
legal(_q,noop) :- role(_q), claiming(_p), distinct(_q,_p).
legal(_p,you_bluff) :- guessing(_p).
legal(_p,ask_roll) :- guessing(_p), \+previous_claimed_values(2,1).
legal(_q,noop) :- role(_q), guessing(_p), distinct(_q,_p).
next(has_dice(_p,_x,_y)) :- does(random,roll(_p,_x1,_y1)), sort(_x1,_y1,_x,_y).
next(claiming(_p)) :- rolling_for(_p).
next(guessing(_q)) :- does(_p,claim(_x,_y)), next_player(_p,_q).
next(previous_claimed_values(_x,_y)) :- does(_p,claim(_x,_y)).
next(rolling_for(_p)) :- does(_p,ask_roll).
next(game_over(_p)) :- does(_p,you_bluff).
next(previous_claimed_values(_x,_y)) :- previous_claimed_values(_x,_y), \+claims_any.
next(has_dice(_p,_x,_y)) :- has_dice(_p,_x,_y), \+any_roll.
sort(_x,_y,_x,_y) :- gteq(_x,_y).
sort(_x,_y,_y,_x) :- \+gteq(_x,_y).
claims_any :- does(_p,claim(_x,_y)).
any_roll :- role(_p), dots(_x), dots(_y), does(random,roll(_p,_x,_y)).
sees(_p,my_dice(_x,_y)) :- does(random,roll(_p,_x,_y)).
sees(_q,does(_p,_m)) :- role(_q), does(_p,_m), distinct(_p,random).
terminal :- game_over(_p).
goal(_q,100) :- game_over(_q), next_player(_p,_q), has_dice(_p,_x,_y), \+previous_claimed_values(_x,_y).
goal(_p,100) :- game_over(_q), next_player(_p,_q), has_dice(_p,_x,_y), previous_claimed_values(_x,_y).
goal(_q,0) :- game_over(_q), next_player(_p,_q), has_dice(_p,_x,_y), previous_claimed_values(_x,_y).
goal(_p,0) :- game_over(_q), next_player(_p,_q), has_dice(_p,_x,_y), \+previous_claimed_values(_x,_y).
goal(random,100).
dots(1).
dots(2).
dots(3).
dots(4).
dots(5).
dots(6).
succ_values(0,0,3,1).
succ_values(3,1,3,2).
succ_values(3,2,4,1).
succ_values(4,1,4,2).
succ_values(4,2,4,3).
succ_values(4,3,5,1).
succ_values(5,1,5,2).
succ_values(5,2,5,3).
succ_values(5,3,5,4).
succ_values(5,4,6,1).
succ_values(6,1,6,2).
succ_values(6,2,6,3).
succ_values(6,3,6,4).
succ_values(6,4,6,5).
succ_values(6,5,1,1).
succ_values(1,1,2,2).
succ_values(2,2,3,3).
succ_values(3,3,4,4).
succ_values(4,4,5,5).
succ_values(5,5,6,6).
succ_values(6,6,2,1).
better_values(_mx,_my,_x,_y) :- succ_values(_mx,_my,_x,_y).
better_values(_mx,_my,_x,_y) :- succ_values(_mx,_my,_ix,_iy), better_values(_ix,_iy,_x,_y).
next_player(p1,p2).
next_player(p2,p1).

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                           GDL helper methods                                                       */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
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


% minmax_goals/3 returns the minimum and maximum goal values for a role.
% This is used to normalize goal values between 0-1.
minmax_goals(Role, Min, Max):-
    findall(Goal, clause(goal(Role, Goal), _), Goals),
    min_list(Goals, Min),
    max_list(Goals, Max).

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                  GDL methods                                                       */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

% legal_jointaction/2 is true whenever JointAction contains one action per role that is legal in the current state.
% e.g. (Tic Tac Toe): legal_jointaction(State, [does(white, mark(1,1)), does(black, noop)])
legal_jointaction(State, JointAction) :-
    roles_pl(Roles),
    legal_jointaction_iter(State, Roles, [], JointAction).

legal_jointaction_iter(_, [], JointActionRev, JointAction) :-
    reverse(JointActionRev, JointAction).
legal_jointaction_iter(State, [RoleH|RoleT], Temp, JointAction) :-
    legal_pl(State, RoleH, Action),
    legal_jointaction_iter(State, RoleT, [Action|Temp], JointAction).

% legal_jointaction_random/2 is used to generate a random JointAction. Usefull for simulations.
legal_jointaction_random(State, JointAction):-
    roles_pl(Roles),
    legal_jointaction_random_iter(State, Roles, [], JointAction).

legal_jointaction_random_iter(_, [], JointAction, JointAction).
legal_jointaction_random_iter(State, [RoleH|RoleT], Temp, JointAction) :-
    legals_pl(State, RoleH, Actions),
    random_member(Action, Actions),
    legal_jointaction_iter(State, RoleT, [Action|Temp], JointAction).


% simulate/3 simulates a random playthrough from the current state until a terminal state is reached.
% It then finds the goal value for the given role in that terminal state.
simulate(State, Role, Value) :-
    (terminal_pl(State) ->
    	goal_pl(State, Role, Value)
    ;
    	legal_jointaction_random(State, JointAction),
    	next_pl(State, JointAction, NextState),
        simulate(NextState, Role, Value)
    ),!.

% simulate/4 simulates multiple random playthroughs from a given state and sums up the total score for the given role.
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

% update_valid_states/5 updates the list of States given the role's Action and Percepts into a list of valid successor states.
% it filters the resulting states on terminality:
%       Terminal == true => StatesOut contains only terminal states
%       Terminal == false => StatesOut contains only non-terminal states
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

% filter_terminals/2 removes all non-terminal states from StatesIn
filter_terminals(StatesIn, StatesOut):-
    filter_terminals(StatesIn, [], StatesOut).
filter_terminals([], StatesOut, StatesOut).
filter_terminals([StatesInH|StatesInT], Temp, StatesOut):-
    (terminal_pl(StatesInH) ->
        filter_terminals(StatesInT, [StatesInH|Temp], StatesOut)
    ;
        filter_terminals(StatesInT, Temp, StatesOut)
    ).

% filter_nonterminals/2 removes all non-terminal states from StatesIn
filter_nonterminals(StatesIn, StatesOut):-
    filter_nonterminals(StatesIn, [], StatesOut).
filter_nonterminals([], StatesOut, StatesOut).
filter_nonterminals([StatesInH|StatesInT], Temp, StatesOut):-
    (terminal_pl(StatesInH) ->
        filter_nonterminals(StatesInT, Temp, StatesOut)
    ;
        filter_nonterminals(StatesInT, [StatesInH|Temp], StatesOut)
    ).

% create_valid_states/5 builds a set of all valid states by updating_valid_states from the start state on with the
% given ActionHist and PerceptHist. On the final set it filters for either terminal or non-terminal states as explained
% in update_valid_states/5
create_valid_states(ActionHist, PerceptHist, StatesOut, Terminal):-
    init_pl(S0),
    create_valid_states([S0], ActionHist, PerceptHist, StatesOut, Terminal).

create_valid_states(StatesIn, [Action], [Percepts], StatesOut, Terminal):-
    update_valid_states(StatesIn, Action, Percepts, StatesOut, Terminal).
create_valid_states(StatesIn, [ActionHistH|ActionHistT], [PerceptHistH|PerceptHistT], StatesOut, Terminal):-
    update_valid_states(StatesIn, ActionHistH, PerceptHistH, NewStatesIn, false),
    create_valid_states(NewStatesIn, ActionHistT, PerceptHistT, StatesOut, Terminal).


% total_goal/2 returns the average goal value of States for the given Role
total_goal(States, Role, Goal):-
    total_goal(States, Role, 0, Goal).
total_goal([], _, Goal, Goal).
total_goal([StatesH|StatesT], Role, Temp, Goal):-
    goal_pl(StatesH, Role, NewGoal),
    NewTemp is Temp + NewGoal,
    total_goal(StatesT, Role, NewTemp, Goal).


%filter_states_percepts/4 filters all states from States that have percepts Percepts when jointactions JointActions are applied.
%returns the indices of the valid states.
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

%a cleaner way to set dynamic facts would be
%:- dynamic(<fact>/<arity>), dynamic(<fact>/<arity>),... .
%but the dynamic clause is unknown to the problog engine
%base
does(_,_) :- fail.
init(_) :- fail.
%chess.gld
or(_,_,_):- fail.
or(_,_,_,_,_) :- fail.
check(_,_,_,_):- fail.
piece_has_moved(_,_,_):- fail.
%blocksworlds.gdl
on(_,_):- fail.
%meier.gdl
claiming(_):- fail.
guessing(_):- fail.
game_over(_):- fail.
has_dice(_,_,_):- fail.
rolling_for(_):- fail.