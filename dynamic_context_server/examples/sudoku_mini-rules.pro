%%  all(ElementClass,ElementAlternative,CurrentSelections,Weight)
%%

all(aa,N,L,9 ) :- 
  L=[N,_,_,_,_,_,_,_,_]  ,fd_all_different(L)
  .

all(ab,N,L,9) :-
  L=[_,N,_,_,_,_,_,_,_]  ,fd_all_different(L)
  .

all(ac,N,L,9) :-
  L=[_,_,N,_,_,_,_,_,_]  ,fd_all_different(L)
  .

all(ba,N,L,9) :-
  L=[_,_,_,N,_,_,_,_,_] ,fd_all_different(L)
  .

all(bb,N,L,9) :-
  L=[_,_,_,_,N,_,_,_,_] ,fd_all_different(L)
  .

all(bc,N,L,9) :-
  L=[_,_,_,_,_,N,_,_,_] ,fd_all_different(L)
  .

all(ca,N,L,9) :-
  L=[_,_,_,_,_,_,N,_,_] ,fd_all_different(L)
  .

all(cb,N,L,9) :-
  L=[_,_,_,_,_,_,_,N,_] ,fd_all_different(L)
  .

all(cc,N,L,9) :-
  L=[_,_,_,_,_,_,_,_,N] ,fd_all_different(L)
  .
