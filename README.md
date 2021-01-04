## **General Game Playing Framework - ProbLog**

This repository is used for my thesis (2020-2021) on extending the General Game Playing 
Game Description Language (GDL) with stochastic actions.
The first part of the thesis revolves around implementing a Python framework to play both GDL and GDL-II games.
To this end, a GameManager and GamePlayer class have been implemented. Both are implemented as HTTPServers.
General Game Players can be written by inheriting from GamePlayer and implementing the three core (timed) methods:
    
    1. player_start
    2. player_play
    3. player_stop

The second part of the thesis revolves around adding to GDL-II the capability to describe stochastic player actions.
Certain constraints are set:
    
    a. Every action has a set of effects with relative probabilities that add up to 1
    b. Only a single effect is chosen each round
    
