role(red).
role(black).
base(cell(_x,_y,_p)) :- x(_x), y(_y), role(_p).
base(control(red)).
base(control(black)).
input(_p,drop(_x)) :- role(_p), x(_x).
input(_p,noop) :- role(_p).
init(control(red)).
legal(red,noop) :- control(black).
legal(red,drop(_x)) :- control(red), columnOpen(_x).
legal(black,noop) :- control(red).
legal(black,drop(_x)) :- control(black), columnOpen(_x).
next(cell(_x,1,_player)) :- does(_player,drop(_x)), columnEmpty(_x).
next(cell(_x,_y2,_player)) :- does(_player,drop(_x)), cellOpen(_x,_y2), succ(_y1,_y2), \+cellOpen(_x,_y1).
next(cell(_x,_y,_player)) :- cell(_x,_y,_player).
next(control(red)) :- control(black).
next(control(black)) :- control(red).
terminal :- line(red).
terminal :- line(black).
terminal :- \+boardOpen.
goal(red,100) :- line(red).
goal(red,50) :- \+line(red), \+line(black), \+boardOpen.
goal(red,0) :- line(black).
goal(red,0) :- \+line(red), \+line(black), boardOpen.
goal(black,100) :- line(black).
goal(black,50) :- \+line(red), \+line(black), \+boardOpen.
goal(black,0) :- line(red).
goal(black,0) :- \+line(red), \+line(black), boardOpen.
cellOpen(_x,_y) :- x(_x), y(_y), \+cell(_x,_y,red), \+cell(_x,_y,black).
columnOpen(_x) :- cellOpen(_x,6).
columnEmpty(_x) :- cellOpen(_x,1).
boardOpen :- x(_x), columnOpen(_x).
line(_player) :- cell(_x1,_y,_player), succ(_x1,_x2), succ(_x2,_x3), succ(_x3,_x4), cell(_x2,_y,_player), cell(_x3,_y,_player), cell(_x4,_y,_player).
line(_player) :- cell(_x,_y1,_player), succ(_y1,_y2), succ(_y2,_y3), succ(_y3,_y4), cell(_x,_y2,_player), cell(_x,_y3,_player), cell(_x,_y4,_player).
line(_player) :- cell(_x1,_y1,_player), succ(_x1,_x2), succ(_x2,_x3), succ(_x3,_x4), succ(_y1,_y2), succ(_y2,_y3), succ(_y3,_y4), cell(_x2,_y2,_player), cell(_x3,_y3,_player), cell(_x4,_y4,_player).
line(_player) :- cell(_x1,_y4,_player), succ(_x1,_x2), succ(_x2,_x3), succ(_x3,_x4), succ(_y3,_y4), succ(_y2,_y3), succ(_y1,_y2), cell(_x2,_y3,_player), cell(_x3,_y2,_player), cell(_x4,_y1,_player).
x(1).
x(2).
x(3).
x(4).
x(5).
x(6).
x(7).
x(8).
y(1).
y(2).
y(3).
y(4).
y(5).
y(6).
cell(_x,_y,_r) :- fail.