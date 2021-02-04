%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                           Dynamic Predicates                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic does/2,  %input
           location/3, step/1, %maze
           cell/3, control/1, %tictactoe
           cell/3, control/1, %connectfour
           clear/1, on/2, table_top/1, step/1, %blocks
           clear/1, on/2, lifted/1, step/1, %blocks2p
           location/3, blockednorth/2, blockedeast/2, step/1, %bomberman
           closed/1, car/1, chosen/1, step/1, %montyhall
           cell/3, tried/3, step/1, %kriegtictactoe
           control/1, location/3, step/1, %transit
           rolling_for/1, previous_claimed_values/2, has_dice/3,claiming/1, guessing/1, game_over/1, %ii_meier.gdl
           turn/1, step/1, cell/3, inPool/2, occupied/3. %stratego