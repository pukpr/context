:- module(context_sos, [generate_sos_profile/3]).

/** <module> Demonstrating superposition of sine waves
    * Fine Terrain
    * from semi-Markov
*/

:- use_module(context_math).


% superposition of sine waves

sos([], _, _, _, Final, Final).
sos([X|Rest], Sx, Phi, Amplitude, Initial, Final) :-
   Coefficient mapdot sin ~> (2*pi*X .* Sx + Phi),
   Z dot Coefficient * Amplitude,
   !,
   sos(Rest, Sx, Phi, Amplitude, [Z|Initial], Final).

%%   generate_sos_profile(+X, +PSD, -Z)
%
%    Generates a SOS profile from a PSD
generate_sos_profile(X, psd(Sx, Sz), Z) :-
   Phi mapdot random_phase ~> Sx,
   Amp mapdot sqrt ~> Sz * Sx,
   sos(X, Sx, Phi, Amp, [], Z).











