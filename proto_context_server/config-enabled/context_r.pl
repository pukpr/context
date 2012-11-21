:- module(context_r, [
		      r_open_session/0
		     ]).

/** <module> Interfacing with R
    * Bessel function
    * Comparison to other math
*/

:- use_module(context_complex).
:- use_module(library('R')).
:- use_module(context_math).

:- op(400, xfx, ~).

r_open_session :-
    % getenv('OS', 'Windows_NT'),
    current_prolog_flag(windows, true),
    r_open([]), !.
r_open_session :-
    r_open([with(non_interactive)]).
