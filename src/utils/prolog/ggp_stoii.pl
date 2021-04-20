%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                          GDL-STO-II Methods                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
legal_actions_states(States, Role, LegalActions):-
    legal_actions_states(States, Role, [], LegalActions).
legal_actions_states([], _,  LegalActions, LegalActions).
legal_actions_states([StatesH|StatesT], Role,  TempLegalActions, LegalActions):-
    legals_pl(StatesH, Role, StateLegalActions),
    ord_union(StateLegalActions, TempLegalActions, NewTempLegalActions),
    legal_actions_states(StatesT, Role, NewTempLegalActions, LegalActions).

%Generates all NewStates that are possible successor states from States
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

update_states_sjas(States, SAction, DAction, Percepts, Terminal, NewStatesSJAS):-
    update_states_sjas_iter(States, SAction, DAction, Percepts, Terminal, [], NewStatesSJAS).
update_states_sjas_iter([], _, _, _, _, NewStatesSJAS, NewStatesSJAS).
update_states_sjas_iter([StatesH|StatesT], SAction, DAction, Percepts, Terminal, TempNewStatesSJAS, FinalNewStatesSJAS):-
    update_state_sjas(StatesH, SAction, DAction, Percepts, Terminal, NewStatesSJAS),
    ord_union(NewStatesSJAS, TempNewStatesSJAS, NewTempNewStatesSJAS),
    update_states_sjas_iter(StatesT, SAction, DAction, Percepts, Terminal, NewTempNewStatesSJAS, FinalNewStatesSJAS).

update_state_sjas(State, does(Role, SAction), DAction, Percepts, Terminal, NewStatesSJAS):-
    (setof(SJA, legal_jointaction(State, SJA), SJAS) *-> true; SJAS = []), %All possible stochastic actions
    (setof(SJA, (member(SJA, SJAS), member(does(Role, SAction), SJA)), SJAS_Filtered) *-> true; SJAS_Filtered = []), %filtered for the ones that have the right action choice for the player role
    (setof(DJA, SJA^(member(SJA, SJAS_Filtered), sja_to_dja(State, SJA, DJA)), DJAS) *-> true; DJAS = []), %all possible outcomes
    (setof(DJA, (member(DJA, DJAS), member(DAction, DJA)), DJAS_Filtered0) *-> true; DJAS_Filtered0 = []), %filtered for the correct outcome of the player
    (setof(DJA, (member(DJA, DJAS_Filtered0), sees_pl(State, DJA, Role, PerceptsState), PerceptsState = Percepts), DJAS_Filtered1) *-> true; DJAS_Filtered1 = []), %filtered for jointactions that deliver the right percepts
    (setof(NextState, DJA^(member(DJA, DJAS_Filtered1), next_pl(State, DJA, NextState)), NextStates) *-> true; NextStates = []),
    (Terminal ->
        (setof(NextState, (member(NextState, NextStates), terminal_pl(NextState)), NextTerminalStates) *-> true; NextTerminalStates = [])
    ;
        (setof(NextState, (member(NextState, NextStates), \+terminal_pl(NextState)), NextTerminalStates) *-> true; NextTerminalStates = [])
    ),
    add_sjas(NextTerminalStates, [], NewStatesSJAS).

add_sjas([], FinalStatesSJAS, FinalStatesSJAS).
add_sjas([StateH|StateT], TempStates, FinalStatesSJAS):-
    add_sjas(StateH, NewStateSJAS),
    add_sjas(StateT, [NewStateSJAS|TempStates], FinalStatesSJAS).
add_sjas(State, StateSJA):-
    legal_jointactions(State, SJAS),
    StateSJA = [State, SJAS].


sja_to_dja(State, SJA, DJA):-
    sja_to_dja_iter(State, SJA, [], DJA).

sja_to_dja_iter(_, [], DJA, DJA).
sja_to_dja_iter(State, [does(Role, SAction)|SJAT], TempDJA, DJA):-
    outcome_pl(State, Role, SAction, Outcomes, _),
    member(DA, Outcomes),
    sja_to_dja_iter(State, SJAT, [DA|TempDJA], DJA).

