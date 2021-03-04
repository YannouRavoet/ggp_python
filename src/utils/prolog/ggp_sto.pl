%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               Game Settings                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_stochastic_actions :-
    clause(outcome(_,_,_,_), _), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                           Helper Methods                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                      Stochastic GDL Methods                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%legal_jointaction_sto/3 finds a legal JointAction *JointAction* with outcomes *Outcomes* in State *State*.
legal_jointaction_sto(State, JointAction, Outcomes):-
    legal_jointaction(State, JointAction),
    findall([DJA, Prob],jointaction_outcome(State, JointAction, DJA, Prob),Outcomes).

%jointaction_outcome/4 returns a DeterministicJointAction that can result from taking JointAction in State with probability Prob.
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

% simulate_sto/3 simulates a random playthrough from the current state until a terminal state is reached.
% It then finds the goal value for the given role in that terminal state.
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

% simulate_sto/4 simulates multiple random playthroughs from a given state and sums up the total score for the given role.
simulate_sto(State, Role, Total, Rounds) :-
    simulate_sto_iter(State, Role, 0, Total, Rounds).

simulate_sto_iter(_, _, Total, Total, 0):- !.
simulate_sto_iter(State, Role, Temp, Total, Rounds):-
    simulate_sto(State, Role, NewValue),
    NewTemp is Temp + NewValue,
    NewRounds is Rounds - 1,
    simulate_sto_iter(State, Role, NewTemp, Total, NewRounds).