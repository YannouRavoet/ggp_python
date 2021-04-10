%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                          GDL-STO-II Methods                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Generates all NewStates that are possible successor states from States
update_states(States, SAction, DAction, Percepts, Terminal, NewStates):-
    update_states_iter(States, SAction, DAction, Percepts, Terminal, [], NewStates).
update_states_iter([], _, _, _, _, NewStates, NewStates).
update_states_iter([StatesH|StatesT], SAction, DAction, Percepts, Terminal, TempNewStates, FinalNewStates):-
    update_state(StatesH, SAction, DAction, Percepts, Terminal, NewStates),
    ord_union(NewStates, TempNewStates, NewTempNewStates),
    update_states_iter(StatesT, SAction, DAction, Percepts, Terminal, NewTempNewStates, FinalNewStates).

update_state(State, SAction, DAction, Percepts, Terminal, NewStates):-
    setof(SJA, legal_jointaction_complete(State, SAction, SJA), SJAS), %filtered so that the player action choice is the same
    setof(DJA, (member(SJA, SJAS), sja_to_dja(State, SJA, DAction, Percepts, DJA)), DJAS), %filtered so that player action outcome is the same and the percepts are the same
    (Terminal ->
        setof(NextState, (member(DJA, DJAS), next_pl(State, DJA, NextState), terminal_pl(NextState)), NewStates)
    ;
        setof(NextState, (member(DJA, DJAS), next_pl(State, DJA, NextState), \+terminal_pl(NextState)), NewStates)
    ).

sja_to_dja(State, SJA, DAction, Percepts, DJA):-
    sja_to_dja_iter(State, SJA, DAction, Percepts, [], DJA).
sja_to_dja_iter(State, [], does(Role, _), Percepts, DJA, DJA):-
    sees_pl(State, DJA, Role, Percepts).
sja_to_dja_iter(State, [does(RoleX, SAction)|SJAT], does(Role, DAction), Percepts, TempDJA, DJA):-
    (Role = RoleX ->
        sja_to_dja_iter(State, SJAT, does(Role, DAction), Percepts, [does(Role, DAction)|TempDJA], DJA)
    ;
        outcome_pl(State, RoleX, SAction, Outcomes, _),
        member(DA, Outcomes),
        sja_to_dja_iter(State, SJAT, does(Role, DAction), Percepts, [DA|TempDJA], DJA)
    ).

