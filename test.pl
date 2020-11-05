role(white).
role(black).
base(cell(_m,_n,x)) :- index(_m), index(_n).
base(cell(_m,_n,o)) :- index(_m), index(_n).
base(cell(_m,_n,b)) :- index(_m), index(_n).
base(control(white)).
base(control(black)).
input(_r,mark(_m,_n)) :- role(_r), index(_m), index(_n).
input(_r,noop) :- role(_r).
index(1).
index(2).
index(3).
init(cell(1,1,b)).
init(cell(1,2,b)).
init(cell(1,3,b)).
init(cell(2,1,b)).
init(cell(2,2,b)).
init(cell(2,3,b)).
init(cell(3,1,b)).
init(cell(3,2,b)).
init(cell(3,3,b)).
init(control(white)).
legal(_w,mark(_x,_y)) :- cell(_x,_y,b), control(_w).
legal(white,noop) :- control(black).
legal(black,noop) :- control(white).
next(cell(_m,_n,x)) :- does(white,mark(_m,_n)), cell(_m,_n,b).
next(cell(_m,_n,o)) :- does(black,mark(_m,_n)), cell(_m,_n,b).
next(cell(_m,_n,_w)) :- cell(_m,_n,_w), \+(=(_w,b)).
next(cell(_m,_n,b)) :- does(_w,mark(_j,_k)), cell(_m,_n,b), \+(=(_m,_j)).
next(cell(_m,_n,b)) :- does(_w,mark(_j,_k)), cell(_m,_n,b), \+(=(_n,_k)).
next(control(white)) :- control(black).
next(control(black)) :- control(white).
goal(white,100) :- line(x), \+line(o).
goal(white,50) :- \+line(x), \+line(o).
goal(white,0) :- \+line(x), line(o).
goal(black,100) :- \+line(x), line(o).
goal(black,50) :- \+line(x), \+line(o).
goal(black,0) :- line(x), \+line(o).
terminal :- line(x).
terminal :- line(o).
terminal :- \+open.
row(_m,_x) :- cell(_m,1,_x), cell(_m,2,_x), cell(_m,3,_x).
column(_n,_x) :- cell(1,_n,_x), cell(2,_n,_x), cell(3,_n,_x).
diagonal(_x) :- cell(1,1,_x), cell(2,2,_x), cell(3,3,_x).
diagonal(_x) :- cell(1,3,_x), cell(2,2,_x), cell(3,1,_x).
line(_x) :- row(_m,_x).
line(_x) :- column(_m,_x).
line(_x) :- diagonal(_x).
open :- cell(_m,_n,b).

distinct(X,Y) :-
    X \= Y.
% Backend methods to more efficiently use the ProbLog engine when having to make multiple calls.

% legal_jointaction\1 returns a random jointaction (one action per role) that is legal in the current state.
legal_jointaction(JointAction) :-
    findall(Role, role(Role), Roles),
	legal_jointaction_iter(Roles, [], JointAction).

legal_jointaction_iter([], Temp, JointAction):-
    reverse(Temp, JointAction).
legal_jointaction_iter([RoleH|RoleT], Temp, JointAction) :-
    findall(Action, legal(RoleH, Action), Actions),
    random_member(Action, Actions),
    legal_jointaction_iter(RoleT, [Action|Temp], JointAction).

legal_jointaction_pl(JointAction) :-
    findall(Role, role(Role), Roles),
    legal_jointaction_iterpl(Roles, [], JointAction).

legal_jointaction_iterpl([], JointAction, JointAction).
legal_jointaction_iterpl([RoleH|RoleT], Temp, JointAction) :-
    findall(Action, legal(RoleH, Action), Actions),
    random_member(Action, Actions),
    legal_jointaction_iterpl(RoleT, [does(RoleH,Action)|Temp], JointAction).


% simulate\2 simulates a random playthrough from the current state until a terminal state is reached.
% It then finds the goal value for the given role in that terminal state.
simulate(State, Role, Value) :-
	maplist(assertz, State),
    (terminal ->
    	goal(Role, Value),
        maplist(retract, State)
    ;
    	legal_jointaction_pl(JointAction),
        maplist(assertz, JointAction),
    	findall(S, next(S),NextState),
        maplist(retract, State),
        maplist(retract, JointAction),
        simulate(NextState, Role, Value)
    ).

simulate(State, Role, AvgValue,Times) :-
    simulate_iter(State, Role, AvgValue, 0, Times, Times).

simulate_iter(_, _, AvgValue, Value, 0, Times):-
    AvgValue is Value/Times.
simulate_iter(State, Role, AvgValue, Total, Left, Times):-
    simulate(State, Role, NewValue),
    NewTotal is Total + NewValue,
    NewLeft is Left - 1,
    simulate_iter(State, Role, AvgValue, NewTotal, NewLeft, Times).