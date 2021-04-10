%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               Game Settings                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_random :-
    role(random).
has_imperfect_information :-
    clause(sees(_,_), _), !.
has_stochastic_actions :-
    clause(action_effects(_,_,_,_), _), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                           Helper Methods                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sees_pl(State, JointAction, Role, Percepts):-
    maplist(assertz, State),
    maplist(assertz, JointAction),
    setof(sees(Role,P), sees(Role,P), Percepts),
    maplist(retract, State),
    maplist(retract, JointAction),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              GDL-II Methods                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

legal_jointaction_states(States, JAS):-
    legal_jointaction_states(States, [], JAS).

legal_jointaction_states([], JAS, JAS).
legal_jointaction_states([StatesH|StatesT], TempJAS, JAS):-
    setof(JA, legal_jointaction(StatesH, JA), StateJAS),
    ord_union(TempJAS, StateJAS, NewTempJAS),
    legal_jointaction_states(StatesT, NewTempJAS, JAS).


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
    generate_jointactions(StatesInH, Action, Percepts, JointActions),
    length(JointActions, Length),
    (Length > 0 -> % If no jointactions were found that result in the same Percepts, the state was not true.
        build_states(StatesInH, JointActions, Temp, NewTemp),
        update_valid_states(StatesInT, Action, Percepts, NewTemp, StatesOut, Terminal)
    ;
        update_valid_states(StatesInT, Action, Percepts, Temp, StatesOut, Terminal)
    ).

% generate_jointactions/4 generates all jointactions that complete the role's Action and result in the given Percepts.
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


% total_goal/2 returns the total goal value of States for the given Role
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

avg_goal_from_history(ActionHist, PerceptHist, Role, AvgGoal):-
    create_valid_states(ActionHist, PerceptHist, StatesOut, true),
    avg_goal(StatesOut, Role, AvgGoal).

%simulate_multi/4 runs Rounds iterations of simulation on each of the provided states and returns a list of values
%for each of the provided States.
simulate_multi(States, Role, Rounds, Values):-
    simulate_multi(States, Role, Rounds, [], Values).

simulate_multi([],_,_,TempValues, Values):-
    reverse(TempValues, Values).
simulate_multi([StatesH|StatesT],Role,Rounds,TempValues, Values):-
    simulate(StatesH, Role, Value, Rounds),
    simulate_multi(StatesT, Role, Rounds, [Value|TempValues], Values).