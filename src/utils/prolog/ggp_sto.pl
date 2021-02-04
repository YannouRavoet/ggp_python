%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               Game Settings                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_stochastic_actions :-
    clause(effect(_,_,_,_), _), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                           Helper Methods                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
effects_pl(State, Role, Action, Effects):-
    maplist(assertz, State),
    effect(Role,Action, Effect, Prob),
    zip(Effect, Prob, Effects),
    maplist(retract, State),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                      Stochastic GDL Methods                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sample_effect(State, Role, Action, Effect):-
    prism(sample_effect(State, Role, Action, Effect)).

sample_effect_jointaction(State, JointAction, Effects):-
    sample_effect_jointaction_iter(State, JointAction, [], Effects).

sample_effect_jointaction_iter(_, [], RevEffects, Effects):-
    reverse(RevEffects, Effects).
sample_effect_jointaction_iter(State, [does(Role, Action) | ActionsT], TempEffects, Effects):-
    sample_effect(State, Role, Action, Effect),
    sample_effect_jointaction_iter(State, ActionsT, [Effect | TempEffects], Effects).

% simulate_sto/3 simulates a random playthrough from the current state until a terminal state is reached.
% It then finds the goal value for the given role in that terminal state.
simulate_sto(State, Role, Value) :-
    (terminal_pl(State) ->
    	goal_pl(State, Role, Value)
    ;
    	legal_jointaction_random(State, JointAction),
    	sample_effect_jointaction(State, JointAction, Effects),
        next_pl(State, Effects, NextState),
        simulate_sto(NextState, Role, Value)
    ),!.

% simulate_sto/4 simulates multiple random playthroughs from a given state and sums up the total score for the given role.
simulate_sto(State, Role, Total, Rounds) :-
    simulate_sto_iter(State, Role, 0, Total, Rounds).

simulate_sto_iter(_, _, Total, Total, 0) :- !.
simulate_sto_iter(State, Role, Temp, Total, Rounds):-
    simulate_sto(State, Role, NewValue),
    NewTemp is Temp + NewValue,
    NewRounds is Rounds - 1,
    simulate_sto_iter(State, Role, NewTemp, Total, NewRounds).