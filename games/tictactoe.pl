role(white).
role(black).
index(1).
index(2).
index(3).
base(cell(_x,_y,b)) :- index(_x), index(_y).
base(cell(_x,_y,x)) :- index(_x), index(_y).
base(cell(_x,_y,o)) :- index(_x), index(_y).
base(control(_p)) :- role(_p).
input(_p,mark(_x,_y)) :- index(_x), index(_y), role(_p).
input(_p,noop) :- role(_p).
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
next(cell(_m,_n,x)) :- does(white,mark(_m,_n)), cell(_m,_n,b).
next(cell(_m,_n,o)) :- does(black,mark(_m,_n)), cell(_m,_n,b).
next(cell(_m,_n,_w)) :- cell(_m,_n,_w), distinct(_w,b).
next(cell(_m,_n,b)) :- does(_w,mark(_j,_k)), cell(_m,_n,b), or(distinct(_m,_j),distinct(_n,_k)).
next(control(white)) :- control(black).
next(control(black)) :- control(white).
row(_m,_x) :- cell(_m,1,_x), cell(_m,2,_x), cell(_m,3,_x).
column(_n,_x) :- cell(1,_n,_x), cell(2,_n,_x), cell(3,_n,_x).
diagonal(_x) :- cell(1,1,_x), cell(2,2,_x), cell(3,3,_x).
diagonal(_x) :- cell(1,3,_x), cell(2,2,_x), cell(3,1,_x).
line(_x) :- row(_m,_x).
line(_x) :- column(_m,_x).
line(_x) :- diagonal(_x).
open :- cell(_m,_n,b).
legal(_w,mark(_x,_y)) :- cell(_x,_y,b), control(_w).
legal(white,noop) :- control(black).
legal(black,noop) :- control(white).
goal(white,100) :- line(x).
goal(white,50) :- \+line(x), \+line(o), \+open.
goal(white,0) :- line(o).
goal(black,100) :- line(o).
goal(black,50) :- \+line(x), \+line(o), \+open.
goal(black,0) :- line(x).
terminal :- line(x).
terminal :- line(o).
terminal :- \+open.