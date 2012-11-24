:- module(context_sos, []).

/** <module> Demonstrating superposition of sine waves
    * Fine Terrain
    * from semi-Markov
*/

:- use_module(context_math).


% superposition of sine waves

sos([], _, _, _, Final, Final).
sos([X|Rest], Sx, Phi, Amp, Initial, Final) :-
   Data mapdot sin ~> (2*pi*X .* Sx + Phi),
   Z dot Data * Amp,
   !,
   sos(Rest, Sx, Phi, Amp, [Z|Initial], Final).














