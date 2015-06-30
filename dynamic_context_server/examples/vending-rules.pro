%% Database
value(nickel, 5).
value(dime, 10).
value(quarter, 25).
value(half_dollar, 50).
value(slug, 0).
product_cost(pretzels, 50).
product_cost(chips, 45).
product_cost(cookies, 55).
product_cost(doughnuts, 60).

%% domain(L) :- member(L,[0,1,2,3,4,5,6,7,8,9]). %, 

change_coin(Coin,Change,N, Rem,Total) :-
   value(Coin,V),
 	Change >= V,
 	C is Change - V,
 	print(Coin),print(' :: '),print(C),nl,!,
   T is N + 1,
 	change_coin(Coin,C,T,Rem,Total).
change_coin(_,Change,Total,Change,Total).

evaluate(N1*nickel+N2*dime+N3*quarter+N4*half_dollar+N5*slug, V) :-
   value(nickel, V1),
   value(dime, V2),
   value(quarter, V3),
   value(half_dollar, V4),
   value(slug, V5),
   fd_domain([N1,N2,N3,N4,N5],0,99),
   fd_labeling([N1,N2,N3,N4,N5]),
   V is V1*N1+V2*N2+V3*N3+V4*N4+V5*N5.

over_pay(product,I,[I,_,_],0).
over_pay(deposit,D,[_,D,_],0).

over_pay(change,Change,[Item,Deposit,_],W) :-
   product_cost(Item, Cost),
   evaluate(Deposit, Money),
   W is Money - Cost,
   W >= 0,
   change_coin(half_dollar, W, 0, W1, HD ),
   change_coin(quarter, W1, 0, W2, Q ),
   change_coin(dime, W2, 0, W3, D ),
   change_coin(nickel, W3, 0, _, N ),
   Change = N*nickel+D*dime+Q*quarter+HD*half_dollar.
   

