:- module(
    ggp,
    [

        %initial_state(State)
        %True if state is the state description of the initial game state.
        initial_state/1,

        % legal_moves(Role, State, Moves)
        % True if Moves contains all legal move for Role to play in State.
        legal_moves/3,

        %state_update(State, Moves, NewState)
        %True if NewState is the result of applying Moves to State
        state_update/3,

        %terminal(State)
        %True if the terminal conditions are satisfied in State
        terminal/1,

        %goal(State, Role, Value)
        %True if in State, Role has a goal value of Value
        goal/3
    ]
).

initial_state(State) :-
    findall(X, init(X), State).

legal_moves(Role, State, Moves) :-
    findall(X, legal(Role, X), Moves).

state_update(State, Moves, NewState):-
    fail.

terminal(State):-
    fail.

goal(State, Role, Value):-
    fail.

legal_moves(Role, State, Moves) :-
    fail.