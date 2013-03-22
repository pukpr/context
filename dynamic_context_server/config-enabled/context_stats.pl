:- module(context_stats, [
		       msv/2
                     ]).

/** <module>  Statistics operations on arrays
*/

:- use_module(context_math).

%%   msv(+L, -X)
%
%    Multi-scale-variance MSV calculates variance on all scales
%
msv(List, Var) :-
    length(List, N),
    Scale is 1/(N-1),
    msv(List, N, [], X),
    Z ordinal X,
    Y mapdot 1.0 .+ Z,
    Vars mapdot Y * X,
    Vars2 mapdot Y * Vars,
    Var mapdot Scale .* Vars2.

msv(_, 0, X, X) :- !.
msv(List, N, X, Y) :-
    findall( Diff,
             (nth1(S,List,S1),
              nth1(T,List,T1),
              N is S-T,
              Diff is (S1-T1)*(S1-T1)),
             Group),
    sumlist(Group, Sum),
    M is N - 1, !,
    msv(List, M, [Sum|X], Y).


