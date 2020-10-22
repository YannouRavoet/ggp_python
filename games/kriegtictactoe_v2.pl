role(white).
role(black).
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
legal(_p,mark(_m,_n)) :- role(_p), cell(_m,_n,_c), \+(=(_c,_p)), \+tried(_p,_m,_n).
doublehit :- does(white,mark(_m,_n)), does(black,mark(_m,_n)).
validmove(_p,_m,_n) :- does(_p,mark(_m,_n)), cell(_m,_n,b), \+doublehit.
marked(_m,_n) :- does(_p,mark(_m,_n)), \+doublehit.
next(cell(_m,_n,_p)) :- validmove(_p,_m,_n).
next(cell(_m,_n,_c)) :- cell(_m,_n,_c), \+(=(_c,b)).
next(cell(_m,_n,b)) :- cell(_m,_n,b), \+marked(_m,_n).
next(tried(_p,_m,_n)) :- does(_p,mark(_m,_n)), \+validmove(_p,_m,_n).
next(tried(_p,_m1,_n1)) :- tried(_p,_m1,_n1), does(_p,mark(_m2,_n2)), \+validmove(_p,_m2,_n2).
next(step(_n)) :- step(_m), succ(_m,_n).
sees(_p,yougotit(_m,_n)) :- validmove(_p,_m,_n).
sees(_p,mark(_m,_n)) :- does(_p,mark(_m,_n)), \+validmove(_p,_m,_n).
terminal :- line(white).
terminal :- line(black).
terminal :- \+open.
terminal :- step(30).
line(_c) :- cell(_m,1,_c), cell(_m,2,_c), cell(_m,3,_c).
line(_c) :- cell(1,_n,_c), cell(2,_n,_c), cell(3,_n,_c).
line(_c) :- cell(1,1,_c), cell(2,2,_c), cell(3,3,_c).
line(_c) :- cell(1,3,_c), cell(2,2,_c), cell(3,1,_c).
open :- cell(_m1,_n1,b), \+tried(white,_m1,_n1), cell(_m2,_n2,b), \+tried(black,_m2,_n2).
goal(white,100) :- line(white).
goal(white,50) :- \+line(white), \+line(black).
goal(white,0) :- line(black).
goal(black,100) :- line(black).
goal(black,50) :- \+line(white), \+line(black).
goal(black,0) :- line(white).
tried(_r,_m,_n) :- fail.