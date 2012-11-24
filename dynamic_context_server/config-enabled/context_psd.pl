:- module(context_psd, [
		       two_level/3
		       ]).

/** <module> PSD models
    * Two level semi-Markov model
    * Markov
*/

:- use_module(context_complex).
:- use_module(context_math).
:- use_module(context_random_walk).

delayed_exponent(L, Alpha, S, R) :-
    Theta is -L*S,
    Num isx 1.0*exp(i*Theta),
    Damp is Alpha*S,
    R isx Num / (1.0 & Damp).

two_level_model(L1, Alpha1, L2, Alpha2, Weight, S, R) :-
    delayed_exponent(L1, Alpha1, S, P),
    delayed_exponent(L2, Alpha2, S, Q),
    % WW is Weight^2,
    WW is Weight^2/sqrt(L1+Alpha1+L2+Alpha2),
    I isx 1.0 & 0.0,
    K isx S   & 0.0,
    W isx WW  & 0.0,
    R & _ isx W*(I-P)*(I-Q)/(I-P*Q)/K^2.

two_level(Course, S, Result) :-  % semiMarkov model
    rdf_(Course, alpha1, Alpha1),
    rdf_(Course, l1, L1),
    rdf_(Course, alpha2, Alpha2),
    rdf_(Course, l2, L2),
    rdf_(Course, weight, Weight),
    two_level_model(L1, Alpha1, L2, Alpha2, Weight, S, Result), !.

two_level(Course, S, Result) :-  % Ornstein-Uhlenbeck
    rdf_(Course, diffusion, Diffusion),
    rdf_(Course, drag, Drag),
    rdf_(Course, spacing, DX),
    rdf_(Course, weight, Weight),
    %Slope is 0.01/sqrt(DX*Diffusion),
    Slope is Drag/sqrt(Diffusion*DX),
    two_level_model(0.0, Slope, 0.0, Slope, Weight, S, Result).











