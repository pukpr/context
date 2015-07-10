power_energy(matlab_calc,1.0,_,X) :- 
   http_query(c52832, 5600, 'ml_dll.set_coord?FT=0.5', inputs(_, _, X)).
power_energy(matlab_calc,2.0,_,X) :-
   http_query(c52832, 5600, 'ml_dll.set_coord?FT=0.4', inputs(_, _, X)).
power_energy(matlab_calc,3.0,_,X) :-
   http_query(c52832, 5600, 'ml_dll.set_coord?FT=0.3', inputs(_, _, X)).
network(matlab_calc,1.0,_,X) :- X is 7.
network(matlab_calc,2.0,_,X) :- X is 9.
network(matlab_calc,3.0,_,X) :- X is 1.

