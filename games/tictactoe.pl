role(white).
role(black).
base(cell(_m,_n,x)) :- index(_m), index(_n).
base(cell(_m,_n,o)) :- index(_m), index(_n).
base(cell(_m,_n,b)) :- index(_m), index(_n).
base(step(1)).
base(step(_n)) :- succ(_m,_n).
input(_role,mark(_m,_n)) :- role(_role), index(_m), index(_n).
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
init(step(1)).
legal(white,mark(_x,_y)) :- true(cell(_x,_y,b)).
legal(black,mark(_x,_y)) :- true(cell(_x,_y,b)).
next(cell(_j,_k,x)) :- true(cell(_j,_k,b)), does(white,mark(_j,_k)), does(black,mark(_m,_n)), (\+(=(_j,_m)); \+(=(_k,_n))).
next(cell(_m,_n,o)) :- true(cell(_m,_n,b)), does(white,mark(_j,_k)), does(black,mark(_m,_n)), (\+(=(_j,_m)); \+(=(_k,_n))).
next(cell(_m,_n,b)) :- true(cell(_m,_n,b)), does(white,mark(_m,_n)), does(black,mark(_m,_n)).
next(cell(_p,_q,b)) :- true(cell(_p,_q,b)), does(white,mark(_j,_k)), does(black,mark(_m,_n)), (\+(=(_j,_p)); \+(=(_k,_q))), (\+(=(_m,_p)); \+(=(_n,_q))).
next(cell(_m,_n,_w)) :- true(cell(_m,_n,_w)), \+(=(_w,b)).
next(step(_y)) :- true(step(_x)), succ(_x,_y).
row(_m,_x) :- true(cell(_m,1,_x)), true(cell(_m,2,_x)), true(cell(_m,3,_x)).
column(_n,_x) :- true(cell(1,_n,_x)), true(cell(2,_n,_x)), true(cell(3,_n,_x)).
diagonal(_x) :- true(cell(1,1,_x)), true(cell(2,2,_x)), true(cell(3,3,_x)).
diagonal(_x) :- true(cell(1,3,_x)), true(cell(2,2,_x)), true(cell(3,1,_x)).
line(_x) :- row(_m,_x).
line(_x) :- column(_m,_x).
line(_x) :- diagonal(_x).
goal(white,100) :- line(x), \+(line(o)).
goal(white,50) :- line(x), line(o).
goal(white,50) :- \+(line(x)), \+(line(o)).
goal(white,0) :- \+(line(x)), line(o).
goal(black,100) :- \+(line(x)), line(o).
goal(black,50) :- line(x), line(o).
goal(black,50) :- \+(line(x)), \+(line(o)).
goal(black,0) :- line(x), \+(line(o)).
terminal :- line(x).
terminal :- line(o).
terminal :- true(step(7)).
succ(1,2).
succ(2,3).
succ(3,4).
succ(4,5).
succ(5,6).
succ(6,7).
