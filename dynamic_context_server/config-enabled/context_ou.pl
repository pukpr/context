:- module(context_ou, [
		       maxent_ou_variance/5,
		       maxent_ou_mean/5
		      ]).

/** <module> Ornstein-Uhlenbeck solution
    * multivariance
    *
*/

:- use_module(library(clpfd)).
:- use_module(library('R')).
:- use_module(context_math).

:- dynamic
   diffElev/4,
   diffusion/1,
   drag/1,
   flatness/1.

/*
%%   ou_variance(+D,+Theta,-X,-Y)
%
%    Ornstein-Uhlenbeck variance
ou_variance(D,Theta,X,Y) :-
   F is (1-exp(-2*X*Theta))/2/Theta,
   Y is sqrt(D*F).
*/
%%   maxent_ou_variance(+D,+Theta,+Flat,-X,-Y)
%
%    Ornstein-Uhlenbeck MaxEnt variance
maxent_ou_variance(_D,_Theta,_Flat,0,0).
maxent_ou_variance(D,Theta,Flatness,X,Y) :-
   F is sqrt(D*(1-exp(-2*X*Theta))/2/Theta),
   H=20,
   % F0 is sqrt((1-exp(-2*40*Theta))/2/Theta),
   % F is FX/F0,
   Var is Flatness*(2*F*F-exp(-H/F)*(2*F*F+2*F*H+H*H))/1,
   Mean is Flatness*(F-exp(-H/F)*(F+H))/1,
   Y is sqrt((Var-Mean*Mean)).

%%   maxent_ou_mean(+D,+Theta,+Flat,+X,-Y)
%
%    Mean for MaxEnt Ornstein-Uhlenbeck
maxent_ou_mean(_D,_Theta,_Flat,0,0).
maxent_ou_mean(D,Theta,Flatness,X,Y) :-
   F is sqrt(D*(1-exp(-2*X*Theta))/2/Theta),
   H=20,
   Y is Flatness*(F-exp(-H/F)*(F+H)).
%   Sum is (1-exp(-H/F)),
%   Y is Mean/Sum.

%%   maxent_ou_sum(+D,+Theta,+Flat,+X,-Y)
%
%    Sum for MaxEnt Ornstein-Uhlenbeck
maxent_ou_sum(_D,_Theta,_Flat,0,0).
maxent_ou_sum(D,Theta,Flatness,X,Y) :-
   F is sqrt(D*(1-exp(-2*X*Theta))/2/Theta),
   H=20,
   Y is Flatness*(1-exp(-H/F)).

%%   ou_model(D,Theta,+X,-Y)
%
%    Ornstein-Uhlenbeck model assert
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


%%   make_df(-X,-Y)
%
%    Labeled search
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

%%   exp_regression(+URI, +X, -Lbar)
%
%    Using R to do a regression fit
exp_regression(URI, X, Lbar) :-
   findall(Result, diffElev(URI, X, Y, Result), YR),
   Y range [1,20]/1,
   YL mapdot log ~> YR,
   y <- YL,
   x <- Y,
   fitxy <- lm('y~x'),
   Slope <- 'as.double(fitxy$coefficients[2])',
   Lbar is 1/Slope.


%%   find_theta(+URI, +X1, +X2, -Theta, -D)
%
%    Find drag value
find_theta(URI, X1, X2, Theta, D) :-
   exp_regression(URI, X1, L1),
   exp_regression(URI, X2, L2),
   Theta is 4*(L1*sqrt(X2)-L2*sqrt(X1))/(X2*sqrt(X2)*L1-X1*sqrt(X1)*L2),
   D is L1^2/X1/(1-Theta*X1/4)^2.

%%   find_avg(+URI, -Theta,-D)
%
%    Find average drag values
find_avg(URI, Theta,D) :-
   X1 in 1..10,
   label([X1]),
   X2 is X1 + 30,
   find_theta(URI, X1, X2, Theta,D).

%%   pair_sum(+List, -Sum)
%
%    Sum pairs in list
%%   pair_summer(+Liswt, +Initial, -Total)
%
%    Pair summer
pair_summer([], Total, Total).
pair_summer([[F1,F2]|R], [Total1,Total2], Final) :-
    Sum1 is Total1 + F1,
    Sum2 is Total2 + F2,
    pair_summer(R, [Sum1,Sum2], Final).

pair_sum(List, Sum) :-
    pair_summer(List, [0,0], Sum).

%%   calc_avg(+URI, -Theta, -Diffusion)
%
%    Calculate average drag and diffusion
calc_avg(URI, Theta, Diffusion) :-
   findall([T,D], find_avg(URI,T,D), Pairs),
   length(Pairs,L),
   pair_sum(Pairs,[TT,DT]),
   Theta is TT/L,
   Diffusion is DT/L.

%%   xrange(-X_low, -X_high)
%
%    X range
xrange(0,40).
%%   y(+URI, +X, -YElev)
%
%    Y elev projection
%%   yrange(-Y_low, -Y_low)
%
%    Y range
yrange(0,20).
%%   scale(-Size)
%
%    Scale of DEM
scale(Size) :- Size is 1201*1201.

y(URI, X, YElev) :-
   yrange(Y0, Y1),
   findall(H, ( between(Y0, Y1, Y),
		context_autocorr:diffElev(URI,X,Y,H)
	      ), L),
   scale(Num),
   Scale is 1/Num,
   YElev mapdot Scale .* L.

%%   var(+URI, +X, -Var)
%
%    Calculate Variance on x slice
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

%%   multivar(+URI, -Vars)
%
%    Do all *var*
multivar(URI, Vars) :-
   xrange(X0, X1),
   findall(Var, ( between(X0, X1, X),
		  var(URI,X,Var)
	      ), Vars).


%%   rms_data(+URI, -Vars)
%
%    Run RMS on section
rms_data(URI, Vars) :-
   context_autocorr:load_dem(URI),
   multivar(URI, Vars).

%%   mean(+URI, +X, -Mean)
%
%    Calculate Mean on x slice
mean(URI, X, Mean) :-
   yrange(Y0, Y1),
   Elev range [Y0,Y1]/1,
   y(URI, X, YElev),
   Mean dot YElev * Elev.

%   sumlist(YElev, Sum), % scaling
%   Mean is M/Sum.

%%   multimean(+URI, -Means)
%
%    Do all *mean*
multimean(URI, Means) :-
   xrange(X0, X1),
   findall(Var, ( between(X0, X1, X),
		  mean(URI,X,Var)
	      ), Means).

%%   mean_data(+URI, -Means)
%
%    Run mean on section
mean_data(URI, Means) :-
   context_autocorr:load_dem(URI),
   multimean(URI, Means).


%%   setup_parameters
%
%    Setup parameters for OU model
setup_parameters :-
    Scale is sqrt(2),
    D range [0.1,4096]^Scale,
    L range [0.00001,1.0]^Scale,
    F range [0.1,1.0]/0.1,
    retractall(diffusion(_)),
    retractall(drag(_)),
    retractall(flatness(_)),
    assert(diffusion(D)),
    assert(drag(L)),
    assert(flatness(F)).


:- setup_parameters.


%%   mean_from_ou(+Data,+Diffusion,+Drag,+Flatness,+Error)
%
%    Calculate Mean on OU model
mean_from_ou(data(X,Y),Diffusion,Drag,Flatness,Error) :-
    diffusion(Diffusion_Parameters),
    member(Diffusion, Diffusion_Parameters),
    drag(Drag_Parameters),
    member(Drag, Drag_Parameters),
    flatness(Flat_Parameters),
    member(Flatness, Flat_Parameters),
    Model mapdot maxent_ou_mean(Diffusion,Drag,Flatness) ~> X,
    Delta mapdot Model - Y,
    Error dot Delta*Delta.

%%   deviation_from_ou(+Data,+Diffusion,+Drag,+Flatness,+Error)
%
%    Calculate RMS on OU model
deviation_from_ou(data(X,Y),Diffusion,Drag,Flatness,Error) :-
    diffusion(Diffusion_Parameters),
    member(Diffusion, Diffusion_Parameters),
    drag(Drag_Parameters),
    member(Drag, Drag_Parameters),
    flatness(Flat_Parameters),
    member(Flatness, Flat_Parameters),
    Model mapdot maxent_ou_variance(Diffusion,Drag,Flatness) ~> X,
    Delta mapdot Model - Y,
    Error dot Delta*Delta.

%%   find_min(L, +Ini, -Min)
%
%    Find minimumum
find_min([], Min, Min).
find_min([[H,_P]|T], Min, Mn) :-
    CN is min(Min, H),
    find_min(T, CN, Mn).
%%   find_minimum(+List, +Min, -Param)
%
%    Find minimumum
find_minimum(List, Min, Param) :-
    List = [[H,_P]|T],
    find_min(T, H, Min),
    member([Min,Param], List).

%%   optimize_rms(+URI, -Diffusion, -Drag, -Flat, -Err)
%
%    Find optimal RMS
optimize_rms(URI, Diffusion, Drag, Flat, Err) :-
    rms_data(URI, Y),
    length(Y,L),
    Len is L - 1,
    X range [0,Len]/1,
    findall([Error, params([diffusion=Di,drag=Dr,flatness=Fl])],
            deviation_from_ou(data(X,Y),Di,Dr,Fl,Error),
            Errors),
    length(Errors, Num),
    print(user_error,['number solutions', Num]),
    find_minimum(Errors, Err, params([diffusion=Diffusion,
                                      drag=Drag,
                                      flatness=Flat])), !.
%%   optimize_mean(+URI, -Diffusion, -Drag, -Flat, -Err)
%
%    Find optimal mean
optimize_mean(URI, Diffusion, Drag, Flat, Err) :-
    mean_data(URI, Y),
    length(Y,L),
    Len is L - 1,
    X range [0,Len]/1,
    findall([Error, params([diffusion=Di,drag=Dr,flatness=Fl])],
            mean_from_ou(data(X,Y),Di,Dr,Fl,Error),
            Errors),
    length(Errors, Num),
    print(user_error,['number solutions', Num]),
    find_minimum(Errors, Err, params([diffusion=Diffusion,
                                      drag=Drag,
                                      flatness=Flat])), !.






