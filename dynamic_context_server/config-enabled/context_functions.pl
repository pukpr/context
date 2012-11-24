:- module(context_functions, [complement/2,
                              random_phase/2,
                              power_law_2_pdf/3,
                              power_law_2_cdf/3,
                              power_law_2_sampling/3,
                              besselk1/3,
                              besselk1_sqrt/3,
			      % used by PDF
			      exp/4,
			      power_law_2/4,
                              besselk0/4,
                              besselk0_sqrt/4,
                              bessel_seastate/6,
			      diffusion_accel/5,
                              corrected_bessel/4,
			      pierson_moskowitz/4,
			      % utility
                              exp/3
                              ]).

/** <module> Function callbacks for modeling
    * Power law PDF
    * Random number generation
    * Exponentials
    *
*/
:- use_module(context_math).
:- use_module(library('R')).

% utility functions
%
complement(X, Y) :-
   number(X),
   Y is 1.0 - X.

% profile generation

random_phase(_,V) :-
   random(R),
   V is 2*pi*R.


exp(Mean, X, Y) :-
   exponential_pdf(Mean, X, Y).
   % Y is exp(-X/Mean)/Mean.


% Sea-State BesselK,0
%
bessel_seastate(Order, Mean, Depth, pdf, X, Y) :-
    nonvar(X),
    Input is 2*X/Mean,
    output <- besselK(Input,Order),
    Output <- output,
    Y is 2*Output/Mean*(1.0-X/Depth).

bessel_seastate(Order, Mean, Depth, cdf, X, Y) :-
    nonvar(X),
    Input is 2*X/Mean,
    Index is Order + 1,
    output <- besselK(Input,Index),
    Output <- output,
    Y is Input*Output*(1.0-X/Depth).

bessel_seastate(_Order, Mean, _Depth, sample, X, Sample) :-
    var(X),
    % Order = 0 is two of these
    random(R1), R1_2 is sqrt(R1),
    random(R2), R2_2 is sqrt(R2),
    Sample is Mean * log(R1_2) * log(R2_2).



% Exponential
%

exp(Mean, pdf, X, Y) :-
   nonvar(X),
   Y is 1.0/Mean * exp(-X/Mean).

exp(Mean, cdf, X, Y) :-
   nonvar(X),
   Y is exp(-X/Mean).

exp(Mean, sample, X, Sample) :-
   var(X),
   random(R),
   Sample is -Mean*log(R).

exp(Mean, symbolic, X, Y) :-
   Y = 1.0/Mean * exp(-X/Mean).



% Power law of order 2
%

power_law_2(Median, pdf, X, Y) :-
   nonvar(X),
   Y = 1.0/Median/(1.0+(X/Median)^2).  % This is symbolic but ends up being evaluated correctly

power_law_2(Median, cdf, X, Y) :-
   nonvar(X),
   Y = 1.0/(1.0+(X/Median)).

power_law_2(Median, sample, X, Sample) :-
   var(X),
   random(R),
   Sample is Median*(1.0/R - 1.0).


% BesselK,0
%
besselk0(Mean, pdf, X, Y) :-
    nonvar(X),
    Input is 2*X/Mean,
    output <- besselK(Input,0),
    Output <- output,
    Y is 2*Output/Mean.

besselk0(Mean, cdf, X, Y) :-
    nonvar(X),
    Input is 2*X/Mean,
    output <- besselK(Input,1),
    Output <- output,
    Y is Input*Output.

besselk0(Mean, sample, X, Sample) :-
    var(X),
    random(R1), R1_2 is sqrt(R1),
    random(R2), R2_2 is sqrt(R2),
    Sample is Mean * log(R1_2) * log(R2_2).


% BesselK,0 sqrt
%
besselk0_sqrt(Mean, pdf, X, Y) :-
    nonvar(X),
    Input is 2*sqrt(X/Mean),
    output <- besselK(Input,0),
    Output <- output,
    Y is 2*Output/Mean.

besselk0_sqrt(Mean, cdf, X, Y) :-
    nonvar(X),
    Input is 2*sqrt(X/Mean),
    output <- besselK(Input,1),
    Output <- output,
    Y is Input*Output.

besselk0_sqrt(Mean, sample, X, Sample) :-
    var(X),
    random(R1),
    random(R2),
    Sample is Mean * log(R1) * log(R2).

% Diffusion Acceleration
%
diffusion_accel(Diff, Accel, pdf, X, Y) :-
    nonvar(X),  % $C$1/(1+$C$2*SQRT($B4)+$C$3*$B4^2)
    Y is 1.0/(1+Diff*sqrt(X)+Accel*(X^2)).

diffusion_accel(Diff, Accel, cdf, X, Y) :-
    nonvar(X),  % -(D/(2 sqrt(X))+2 A X)/(1+D sqrt(X)+A X^2)^2
    Y is Diff/(2*sqrt(X)+2*Accel*X)/((1+Diff*sqrt(X)+Accel*(X^2)))^2.

diffusion_accel(Diff, Accel, sample, X, Sample) :-
    var(X),
    random(R1),
    random(R2),
    Sample is R1*Diff + R2*Accel.  % Not done -----------------------------------



% pierson_moskowitz

pierson_moskowitz(Mean, pdf, X, Y) :-
    nonvar(X),  % 1/f^5*exp(-c/f^4)
    Y is 4.0*((Mean/X)^5)/Mean*exp(-((Mean/X)^4)).

pierson_moskowitz(Mean, cdf, X, Y) :-
    nonvar(X),  %
    Y is 1-exp(-Mean/(X^4)).

pierson_moskowitz(Mean, sample, X, Sample) :-
    var(X),
    random(R),
    Sample is (-Mean/log(R))^0.25.



% ---------------------------------------------------------------------------------
%
% Placeholder

besselk1(Mean, X, Y) :-
    % M is 1.0/Mean,
    % S mapdot M .* X,
    % Input mapdot 2*sqrt ~> S,
    Input is 2*sqrt(X/Mean),
    output <- besselK(Input,0),
    Output <- output,
    Y is 2*Output/Mean.
%   power_law_2_pdf(Mean, X, Y).

corrected_bessel(Mean, _Depth, X, Y) :-
   power_law_2_pdf(Mean, X, Y).

besselk1_sqrt(Mean, X, Y) :-
   power_law_2_pdf(Mean, X, Y).

% Power law of order 2
% %%%%%%%%%%%%%%%%%%%%

power_law_2_pdf_symbolic(Median, X, Y) :-
   Y = 1.0/Median/(1.0+(X/Median)^2).

power_law_2_pdf(Median, X, Y) :-
   number(Median),
   power_law_2_pdf_symbolic(Median, X, Y0),
   Y is Y0.

power_law_2_cdf_symbolic(Median, X, Y) :-
   Y = 1.0/(1.0+(Median/X)).

power_law_2_cdf(Median, X, Y) :-
   number(Median),
   power_law_2_cdf_symbolic(Median, X, Y0),
   Y is Y0.

power_law_2_sampling(Median,_,Variate) :-
   random(R),
   Variate is Median/(1.0/R-1.0).


% Exponential
% %%%%%%%%%%%
exponential_pdf_symbolic(Mean, X, Y) :-
    Y = 1.0/Mean * exp(-X/Mean).

exponential_pdf(Mean, X, Y) :-
    number(Mean),
    exponential_pdf_symbolic(Mean, X, Y0),
    Y is Y0.

exponential_cdf_symbolic(Mean, X, Y) :-
    Y = 1- exp(-X/Mean).

exponential_cdf(Mean, X, Y) :-
    number(Mean),
    exponential_cdf_symbolic(Mean, X, Y0),
    Y is Y0.

exponential_sampling(Mean,_,Variate) :-
    random(R),
    Variate is -Mean * log(R).

% Bessel
% %%%%%%

besselk_1_cdf(Mean, X_Array, Y_Array) :-
    context_r:r_open_session,
    Input mapdot sqrt ~> (2/Mean) .* X_Array,
    y <- besselK(Input,1),
    Y <- y,
    Y_Array mapdot complement ~> Input * Y,
    r_close.

besselk_0_pdf(Mean, X_Array, Y_Array) :-
    context_r:r_open_session,
    Input mapdot sqrt ~> (2/Mean) .* X_Array,
    y <- besselK(Input,0),
    Y <- y,
    Y_Array mapdot (1/Mean) .* Y,
    r_close.

besselk_1_sampling(Mean,_,Variate) :-
    random(R1),
    random(R2),
    Variate is Mean * log(R1) * log(R2).


% X range [0.001,10.0]^(2.0), context_functions:besselk_0_pdf(1.0,X,Y),
% Z integrate Y*X, context_functions:besselk_1_cdf(2.0,X,Q), last(Z,L),
% last(Q,QL).

% Add Rayleigh, some waves
