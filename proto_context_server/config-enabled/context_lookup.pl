:- module(context_lookup, [lookup_table/5]).

/** <module> Lookup algorithms
    * Table interpolation
    *
*/

:- use_module(context_math).

nth(Input, Table, Output) :-
   nth0(Input, Table, Output), !.
nth(_I, _Table, 0.0).

% uses 5-point Lagrange interpolation.
%
lookup_table(Scale, Offset, Table, Input, Output) :-
    I is floor(Scale*(Input-Offset)),
    P is Scale*(Input-Offset)-I,
    P2m1 is P*P - 1.0,
    P2m4 is P2m1 - 3.0,
    Index is I + 2,
    Im2 is Index - 2,
    Im1 is Index - 1,
    Ip1 is Index + 1,
    Ip2 is Index + 2,
    nth(Im2,   Table, Tm2),
    nth(Im1,   Table, Tm1),
    nth(Index, Table, T0),
    nth(Ip1,   Table, Tp1),
    nth(Ip2,   Table, Tp2),
    Output is (P2m1*P*(P-2)*Tm2/24.0 - (P-1)*P*P2m4*Tm1/6.0 + P2m1*P2m4*T0/4.0
              - (P+1)*P*P2m4*Tp1/6.0 + P2m1*P*(P+2)*Tp2/24.0).


lookup_list(xlist=Xlist,
            ylist=Ylist,
            x=X,
            y=Y) :-
   index(Xlist, X, Index),
   spatial(Ylist, Index, Y).
