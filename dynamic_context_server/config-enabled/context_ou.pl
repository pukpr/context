:- module(context_ou, [
		       maxent_ou_variance/4
		      ]).

/** <module> Ornstein-Uhlenbeck solution
    * multivariance
    *
*/

:- use_module(library(clpfd)).
:- use_module(library('R')).
:- use_module(context_math).

:- dynamic
   diffElev/4.

/*
ou_variance(D,Theta,X,Y) :-
   F is (1-exp(-2*X*Theta))/2/Theta,
   Y is sqrt(D*F).
*/
maxent_ou_variance(_D,_Theta,0,0).
maxent_ou_variance(D,Theta,X,Y) :-
   F is sqrt(D*(1-exp(-2*X*Theta))/2/Theta),
   H=20,
   % F0 is sqrt((1-exp(-2*40*Theta))/2/Theta),
   % F is FX/F0,
   Var is (2*F*F-exp(-H/F)*(2*F*F+2*F*H+H*H))/1,
   Mean is (F-exp(-H/F)*(F+H))/1,
   Y is sqrt((Var-Mean*Mean)).


ou_model(_D,_Theta,0,0) :-
   assert(diffElev(zzz, 0, 0, 1)), !.
ou_model(_D,_Theta,0,Y) :-
   R is 1/Y,
   assert(diffElev(zzz, 0, 0, R)), !.
ou_model(D,Theta,X,Y) :-
   F is D/Theta*(1-exp(-X*Theta)),
   Result is 1/sqrt(F)*exp(-Y/sqrt(F)),
   R is integer(1201*1201*Result),
   assert(diffElev(zzz, X, Y, R)).


make_df(X,Y) :-
   X in 1..40,
   Y in 0..20,
   label([X,Y]),
   ou_model(20.0, 0.01, X, Y).

% :- findall([X,Y], make_df(X,Y), _).

/*
multi_regression(Lbar) :-
   X in 1..40,
   label([X]),
   exp_regression(X, Lbar).

mr(LS) :-
   findall(L, multi_regression(L), LS).
*/

exp_regression(URI, X, Lbar) :-
   findall(Result, diffElev(URI, X, Y, Result), YR),
   Y range [1,20]/1,
   YL mapdot log ~> YR,
   y <- YL,
   x <- Y,
   fitxy <- lm('y~x'),
   Slope <- 'as.double(fitxy$coefficients[2])',
   Lbar is 1/Slope.


find_theta(URI, X1, X2, Theta, D) :-
   exp_regression(URI, X1, L1),
   exp_regression(URI, X2, L2),
   Theta is 4*(L1*sqrt(X2)-L2*sqrt(X1))/(X2*sqrt(X2)*L1-X1*sqrt(X1)*L2),
   D is L1^2/X1/(1-Theta*X1/4)^2.

find_avg(URI, Theta,D) :-
   X1 in 1..10,
   label([X1]),
   X2 is X1 + 30,
   find_theta(URI, X1, X2, Theta,D).

pair_summer([], Total, Total).
pair_summer([[F1,F2]|R], [Total1,Total2], Final) :-
    Sum1 is Total1 + F1,
    Sum2 is Total2 + F2,
    pair_summer(R, [Sum1,Sum2], Final).

pair_sum(List, Sum) :-
    pair_summer(List, [0,0], Sum).

calc_avg(URI, Theta, Diffusion) :-
   findall([T,D], find_avg(URI,T,D), Pairs),
   length(Pairs,L),
   pair_sum(Pairs,[TT,DT]),
   Theta is TT/L,
   Diffusion is DT/L.

xrange(0,40).
yrange(0,20).
scale(Size) :- Size is 1201*1201.

y(URI, X, YElev) :-
   yrange(Y0, Y1),
   findall(H, ( between(Y0, Y1, Y),
		context_autocorr:diffElev(URI,X,Y,H)
	      ), L),
   scale(Num),
   Scale is 1/Num,
   YElev mapdot Scale .* L.

var(URI, X, Var) :-
   yrange(Y0, Y1),
   Elev range [Y0,Y1]/1,
   % Z range [0,39]/2,
   % Z0 mapdot 1 .+ Z,
   % Z1 mapdot 0.5 .* Z0,
   % Elev = [0|Z1],
   y(URI, X, YElev),
   Sq mapdot Elev * Elev,
   Var0 dot YElev * Sq,
   Mean dot YElev * Elev,
   Var is sqrt(Var0 - Mean*Mean).

multivar(URI, Vars) :-
   xrange(X0, X1),
   findall(Var, ( between(X0, X1, X),
		  var(URI,X,Var)
	      ), Vars).


run(URI, Vars) :-
   context_autocorr:load_dem(URI),
   multivar(URI, Vars).
