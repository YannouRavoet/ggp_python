role(random).
role(p1).
role(p2).
init(rolling_for(p1)).
init(previous_claimed_values(0,0)).
legal(random,roll(_p,_x,_y)) :- rolling_for(_p), dots(_x), dots(_y).
legal(_p,noop) :- role(_p), \+(=(_p,random)), rolling_for(_q).
legal(_p,claim(_x,_y)) :- claiming(_p), previous_claimed_values(_mx,_my), better_values(_mx,_my,_x,_y).
legal(_q,noop) :- role(_q), claiming(_p), \+(=(_q,_p)).
legal(_p,you_bluff) :- guessing(_p).
legal(_p,ask_roll) :- guessing(_p), \+previous_claimed_values(2,1).
legal(_q,noop) :- role(_q), guessing(_p), \+(=(_q,_p)).
next(has_dice(_p,_x,_y)) :- does(random,roll(_p,_x1,_y1)), sort(_x1,_y1,_x,_y).
next(claiming(_p)) :- rolling_for(_p).
next(guessing(_q)) :- does(_p,claim(_x,_y)), next_player(_p,_q).
next(previous_claimed_values(_x,_y)) :- does(_p,claim(_x,_y)).
next(rolling_for(_p)) :- does(_p,ask_roll).
next(game_over(_p)) :- does(_p,you_bluff).
next(previous_claimed_values(_x,_y)) :- previous_claimed_values(_x,_y), \+claims_any.
next(has_dice(_p,_x,_y)) :- has_dice(_p,_x,_y), ;, stays, true, until, some, other, dice, are, rolled, \+any_roll.
sort(_x,_y,_x,_y) :- leq(_y,_x).
sort(_y,_x,_x,_y) :- leq(_y,_x).
claims_any :- does(_p,claim(_x,_y)).
any_roll :- role(_p), dots(_x), dots(_y), does(random,roll(_p,_x,_y)).
sees(_p,my_dice(_x,_y)) :- does(random,roll(_p,_x,_y)).
sees(_q,does(_p,_m)) :- role(_q), does(_p,_m), \+(=(_p,random)).
terminal :- game_over(_p).
goal(_q,100) :- game_over(_q), next_player(_p,_q), has_dice(_p,_x,_y), \+previous_claimed_values(_x,_y).
goal(_p,100) :- game_over(_q), next_player(_p,_q), has_dice(_p,_x,_y), previous_claimed_values(_x,_y).
goal(_q,0) :- game_over(_q), next_player(_p,_q), has_dice(_p,_x,_y), previous_claimed_values(_x,_y).
goal(_p,0) :- game_over(_q), next_player(_p,_q), has_dice(_p,_x,_y), \+previous_claimed_values(_x,_y).
goal(random,100).
dots(1).
dots(2).
dots(3).
dots(4).
dots(5).
dots(6).
leq(_x,_x) :- dots(_x).
leq(_x,_y) :- succ(_x,_i), leq(_i,_y).
succ_values(0,0,3,1).
succ_values(3,1,3,2).
succ_values(3,2,4,1).
succ_values(4,1,4,2).
succ_values(4,2,4,3).
succ_values(4,3,5,1).
succ_values(5,1,5,2).
succ_values(5,2,5,3).
succ_values(5,3,5,4).
succ_values(5,4,6,1).
succ_values(6,1,6,2).
succ_values(6,2,6,3).
succ_values(6,3,6,4).
succ_values(6,4,6,5).
succ_values(6,5,1,1).
succ_values(1,1,2,2).
succ_values(2,2,3,3).
succ_values(3,3,4,4).
succ_values(4,4,5,5).
succ_values(5,5,6,6).
succ_values(6,6,2,1).
better_values(_mx,_my,_x,_y) :- succ_values(_mx,_my,_x,_y).
better_values(_mx,_my,_x,_y) :- succ_values(_mx,_my,_ix,_iy), better_values(_ix,_iy,_x,_y).
next_player(p1,p2).
next_player(p2,p1).