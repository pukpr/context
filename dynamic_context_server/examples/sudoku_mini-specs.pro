criteria(all,1).

domain(L) :- L=[1,2,3,4,5,6,7,8,9], fd_domain(L,1,9).
%% domain(L) :- [1,2,3,4,5,6,7,8,9].

element(aa, L) :- domain(L).
element(ab, L) :- domain(L).
element(ac, L) :- domain(L).
element(ba, L) :- domain(L).
element(bb, L) :- domain(L).
element(bc, L) :- domain(L).
element(ca, L) :- domain(L).
element(cb, L) :- domain(L).
element(cc, L) :- domain(L).

