%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                           Dynamic Predicates                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic does/2,  %input
           loc/3, step/1, %maze
           loc/3, step/1, has_sword/0, robot_dead/0,%maze_guarded
           loc/3, control/1, %tictactoe
           loc/3, control/1, %connectfour
           clear/1, on/2, table_top/1, step/1, %blocks
           clear/1, on/2, lifted/1, step/1, %blocks2p
           location/3, blockednorth/2, blockedeast/2, step/1, %bomberman
           closed/1, car/1, chosen/1, step/1, %montyhall
           location/3, tried/3, step/1, %kriegtictactoe
           control/1, location/3, step/1, %transit
           rolling_for/1, previous_claimed_values/2, has_dice/3,claiming/1, guessing/1, game_over/1, %ii_meier.gdl
           turn/1, step/1, location/3, inPool/2, occupied/3,%stratego
           control/1, location/3, moved/2, %amazons
           die/3, control/1, step/1, throwndie/2, %dicegame
           loc/3, step/1, control/1, %kttt_sto
           asked/0, exploded/0, step/1, armed/1. %explodingbomb