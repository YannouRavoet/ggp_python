role(white).
role(black).
role(random).
index(1).
index(2).
index(3).
base(cell(_m,_n,x)) :- index(_m), index(_n).
base(cell(_m,_n,o)) :- index(_m), index(_n).
base(cell(_m,_n,b)) :- index(_m), index(_n).
base(tried(white,_m,_n)) :- index(_m), index(_n).
base(tried(black,_m,_n)) :- index(_m), index(_n).
input(white,mark(_m,_n)) :- index(_m), index(_n).
input(black,mark(_m,_n)) :- index(_m), index(_n).
input(random,tiebreak(x)).
input(random,tiebreak(0)).
init(cell(1,1,b)).
init(cell(1,2,b)).
init(cell(1,3,b)).
init(cell(2,1,b)).
init(cell(2,2,b)).
init(cell(2,3,b)).
init(cell(3,1,b)).
init(cell(3,2,b)).
init(cell(3,3,b)).
legal(white,mark(_m,_n)) :- index(_m), index(_n), \+tried(white,_m,_n).
legal(black,mark(_m,_n)) :- index(_m), index(_n), \+tried(black,_m,_n).
legal(random,tiebreak(x)).
legal(random,tiebreak(0)).
next(tried(_p,_m,_n)) :- does(_p,mark(_m,_n)).
next(tried(_r,_m,_n)) :- tried(_r,_m,_n).
next(cell(_m,_n,x)) :- does(white,mark(_m,_n)), cell(_m,_n,b), does(black,mark(_j,_k)), (\+(=(_m,_j)); \+(=(_n,_k))).
next(cell(_m,_n,o)) :- does(black,mark(_m,_n)), cell(_m,_n,b), does(white,mark(_j,_k)), (\+(=(_m,_j)); \+(=(_n,_k))).
next(cell(_m,_n,_w)) :- cell(_m,_n,b), does(black,mark(_m,_n)), does(white,mark(_m,_n)), does(random,tiebreak(_w)).
next(cell(_m,_n,x)) :- cell(_m,_n,x).
next(cell(_m,_n,o)) :- cell(_m,_n,o).
next(cell(_m,_n,b)) :- cell(_m,_n,b), \+marked(_m,_n).
marked(_m,_n) :- does(_r,mark(_m,_n)).
percept(white,ok).
percept(black,ok).
sees(_r,ok) :- does(_r,mark(_m,_n)), cell(_m,_n,b), does(_s,mark(_j,_k)), \+(=(_r,_s)), (\+(=(_m,_j)); \+(=(_n,_k))).
sees(white,ok) :- does(white,mark(_m,_n)), cell(_m,_n,b), does(random,tiebreak(x)).
sees(black,ok) :- does(black,mark(_m,_n)), cell(_m,_n,b), does(random,tiebreak(o)).
goal(white,100) :- line(x), \+line(o).
goal(white,50) :- line(x), line(o).
goal(white,50) :- \+open, \+line(x), \+line(o).
goal(white,0) :- line(o), \+line(x).
goal(black,100) :- line(o), \+line(x).
goal(black,50) :- line(x), line(o).
goal(black,50) :- \+open, \+line(x), \+line(o).
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