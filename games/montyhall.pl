role(candidate).
role(random).
init(closed(1)).
init(closed(2)).
init(closed(3)).
init(step(1)).
legal(random,hide_car(_d)) :- step(1), closed(_d).
legal(random,open_door(_d)) :- step(2), closed(_d), \+car(_d), \+chosen(_d).
legal(random,noop) :- step(3).
legal(candidate,choose(_d)) :- step(1), closed(_d).
legal(candidate,noop) :- step(2).
legal(candidate,switch) :- step(3).
legal(candidate,noop) :- step(3).
sees(candidate,does(candidate,_m)) :- does(candidate,_m).
sees(candidate,open_door(_d)) :- does(random,open_door(_d)).
sees(candidate,car(_d)) :- step(3), car(_d), next_chosen(_d).
next(car(_d)) :- does(random,hide_car(_d)).
next(car(_d)) :- car(_d).
next(closed(_d)) :- closed(_d), \+does(random,open_door(_d)).
next(chosen(_d)) :- next_chosen(_d).
next_chosen(_d) :- does(candidate,choose(_d)).
next_chosen(_d) :- chosen(_d), \+does(candidate,switch).
next_chosen(_d) :- does(candidate,switch), closed(_d), \+chosen(_d).
next(step(2)) :- step(1).
next(step(3)) :- step(2).
next(step(4)) :- step(3).
terminal :- step(4).
goal(random,100).
goal(candidate,100) :- chosen(_d), car(_d).
goal(candidate,0) :- chosen(_d), \+car(_d).
car(_d) :- fail.
chosen(_d) :- fail.
sees(random,does(_r,_m)) :- does(_r,_m).