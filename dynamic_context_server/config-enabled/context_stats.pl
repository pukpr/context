:- module(context_stats, [
		       msv/2,
                       expsm/3,
                       absdev/3,
                       dist/3,
                       mean/2,
                       moment/3,
                       median/2,
                       rms/2,
                       corrcoeff/3,
		       sum_of_squares/3,
		       median_filter/2
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
    % Scale is 1/(N-1),
    msv(List, N, N, [], Var).
    % Z ordinal X,
    % Y mapdot 1.0 .+ Z,
    % Var mapdot Scale .* X.
    %Vars mapdot Y * X,
    %Vars2 mapdot Y * Vars,
    %Var mapdot Scale .* Vars2.

msv(_, 0, _Max, X, X) :- !.
msv(List, N, Max, X, Y) :-
    findall( Diff,
             (nth1(S,List,S1),
              nth1(T,List,T1),
              N is S-T,
              Diff is (S1-T1)*(S1-T1)),
             Group),
    sumlist(Group, S),
    Sum is S/(Max-N+1),
    M is N - 1, !,
    msv(List, M, Max, [Sum|X], Y).


%%   expsm(+X, +A, -Y)
%
%    Exponential smoother
%
expsm([], _, _, Y_In, Y_Out) :- reverse(Y_In, Y_Out).
expsm([X|X_In], A, Sum, Y_In, Y_Out) :-
    Y is (1-A)*X + A*Sum,
    expsm(X_In, A, Y, [Y|Y_In], Y_Out).

expsm(X, A, Y) :-
    expsm(X, A, 0.0, [], Y).



%%   absdev(+X, +A, -Y)
%
%    Absolute deviation from scalar A
%
absdev([],_,Y, Y).
absdev([X|X_In],A,Y_In, Y_Out) :-
    Y is abs(X-A) + Y_In,
    absdev(X_In,A,Y, Y_Out).

absdev(X, A, Y) :-
    absdev(X,A,0,Y).

%%   dist(+X, +A, -Y)
%
%    Norm squared deviation from scalar A
%
dist([],_,Y, Y).
dist([X|X_In],A,Y_In, Y_Out) :-
    Y is (X-A)*(X-A) + Y_In,
    dist(X_In,A,Y, Y_Out).

dist(X, A, Y) :-
    dist(X,A,0,Y).

%%   mean(+X, -Y)
%
%    Mean of array
%
mean([],Y, Y).
mean([X|X_In],Y_In, Y_Out) :-
    Y is X + Y_In,
    mean(X_In,Y, Y_Out).

mean(X, Y) :-
    length(X, N),
    mean(X, 0.0, Y0),
    Y is Y0/N.


%%   moment(+X, +A, -Y)
%
%    Moment A of array
%
moment([],_,Y, Y).
moment([X|X_In],A,Y_In, Y_Out) :-
    Y is X**A + Y_In,
    moment(X_In, A, Y, Y_Out).

moment(X, A, Y) :-
    length(X, N),
    moment(X, A, 0.0, Y0),
    Y is Y0/N.

%%   median(+X, -Y)
%
%    Median of array
%
median(X, Y) :-
    msort(X, XS),
    length(X, N),
    NF is N/2+1,
    N2 is floor(NF),
    nth1(N2, XS, Y).

%%   rms(+X, -Y)
%
%    RMS of array
%
rms(X, Y) :-
    mean(X, M),
    length(X, N),
    dist(X, M, Y0),
    Y is sqrt(Y0/N).

%%   corrcoef(+X, +Y, -R)
%
%    Correlation Coefficient of two arrays
%
corrcoeff(X, Y, R) :-
    mean(X, XM), XOff is -XM,
    mean(Y, YM), YOff is -YM,
    DXM mapdot XOff .+ X,
    DYM mapdot YOff .+ Y,
    Num dot DXM*DYM,
    XDen dot DXM*DXM,
    YDen dot DYM*DYM,
    Den is sqrt(XDen*YDen),
    R is Num/Den.

%%   sum_of_squares(+X, +Y, -R)
%
%    Sum of squares of differences between two arrays
%
sum_of_squares(X, Y, R) :-
    Diffs difference X - Y,
    SS mapdot Diffs * Diffs,
    sumlist(SS, R).


%%   median_filter(+X,-Y)
%
%   Do a near-neighbor median filter on a list.
%

sorter(L, V) :- sort(L,[_,V,_]).
sorter([F,F,_], F).
sorter([F,_,F], F).
sorter([_,F,F], F).

median_filter([X|Rest], Y) :-
    median_filter(X, Rest, [X], Y), !.

median_filter(_,[L], In, Y) :-
    reverse([L|In], Y), !.
median_filter(F,[M,L], In, Y) :-
    sorter([F,M,L],V),
    median_filter(M, [L], [V|In], Y).
median_filter(F, [M,L,R|Rest], In, Y) :-
    sorter([F,M,L],V),
    median_filter(M,[L,R|Rest], [V|In], Y).
