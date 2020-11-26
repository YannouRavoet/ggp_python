%a cleaner way to set dynamic facts would be
%:- dynamic(<fact>/<arity>), dynamic(<fact>/<arity>),... .
%but the dynamic clause is unknown to the problog engine
%base
does(_,_) :- fail.
init(_) :- fail.
%chess.gld
or(_,_,_):- fail.
or(_,_,_,_,_) :- fail.
check(_,_,_,_):- fail.
piece_has_moved(_,_,_):- fail.
%blocksworlds.gdl
on(_,_):- fail.
%meier.gdl
claiming(_):- fail.
guessing(_):- fail.
game_over(_):- fail.
has_dice(_,_,_):- fail.
rolling_for(_):- fail.