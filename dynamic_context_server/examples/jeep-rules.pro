cost(spare_tires,0*spare,_,X) :- X is 1.
cost(spare_tires,1*spare,_,X) :- X is 5.
cost(topper,hard_top,_,X) :- X is 2.
cost(topper,no_top,_,X) :- X is 8.

reliability(spare_tires,0*spare,_,X) :-
   http_query(c53060, 5600, 'carms_extern.command?GET_PROB=2', R),
   X is 1.0/R.

reliability(spare_tires,1*spare,_,X) :-
   http_query(c53060, 5600, 'carms_extern.command?GET_PROB=5', R),
   X is 1.0/R.

reliability(topper,hard_top,_,X) :- X is 3.
reliability(topper,no_top,_,X) :- X is 5.

mobility(spare_tires,0*spare,_,X) :- X is 2.
mobility(spare_tires,1*spare,_,X) :- X is 1.
mobility(topper,hard_top,_,X) :- X is 4.
mobility(topper,no_top,_,X) :- X is 6.
weight(spare_tires,0*spare,_,X) :- X is 2.
weight(spare_tires,1*spare,_,X) :- X is 1.
weight(topper,hard_top,_,X) :- X is 2.
weight(topper,no_top,_,X) :- X is 8.
sustainability(spare_tires,0*spare,_,X) :- X is 3.
sustainability(spare_tires,1*spare,_,X) :- X is 1.
sustainability(topper,hard_top,_,X) :- X is 3.
sustainability(topper,no_top,_,X) :- X is 1.
power_energy(spare_tires,0*spare,_,X) :- X is 2.
power_energy(spare_tires,1*spare,_,X) :- X is 1.
power_energy(topper,hard_top,_,X) :- X is 5.
power_energy(topper,no_top,_,X) :- X is 2.
