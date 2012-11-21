:- module(context_sos, []).

/** <module> Demonstrating superposition of sine waves
    * Fine Terrain
    * from semi-Markov
*/

:- use_module(context_math).

:- context:register(context_sos:sos).
:- context:register(context_sos:sos_config).

% superposition of sine waves

sos([], _, _, _, Final, Final).
sos([X|Rest], Sx, Phi, Amp, Initial, Final) :-
   Data mapdot sin ~> (2*pi*X .* Sx + Phi),
   Z dot Data * Amp,
   !,
   sos(Rest, Sx, Phi, Amp, [Z|Initial], Final).

generate_random_walk_1(X, Z) :-
   % Real space
   linear_range(0, 999, Dx),
   X mapdot 0.01 .* Dx,

   % Reciprocal space
   % Create a log frequency range and apply a PSD & Phase
   % Apply sqrt to intensity density
   log_range(0.01, 10.0, 0.6, Sx),
   PSD mapdot power_law_2_pdf(100.0) ~> Sx,
   Phi mapdot random_phase ~> Sx,
   Amp mapdot sqrt ~> PSD * Sx,
   sos(X, Sx, Phi, Amp, [], Z).

generate_random_walk_2(Period, X, Z) :-
   % Real space
   linear_range(0, 999, Dx),
   X mapdot 0.01 .* Dx,

   % Reciprocal space
   context_r_demo:builtin_complex(Period, linear, Sx, PSD),
   Phi mapdot random_phase ~> Sx,
   Amp mapdot sqrt ~> PSD * Sx,
   sos(X, Sx, Phi, Amp, [], Z).

% param(length, [float]).

sos(Request) :-
   http_parameters(Request, []),
   generate_random_walk_1(X, Z),
   context_r_demo:construct_psd(X, Z, '"X, Z\\n"', Out),
   reply_html_page(% cliopatria(default),
                   [title('Superposition of sines from PSD'),
                    \(con_text:style_cliopatria)
                    % \(context_graphing:dygraph_script_load)
                   ],
                   [
                    \(context_graphing:dygraph_plot( false,
                                                     '',
                                                     'x',
                                                     'z',
                                                     'Superposition of sines from PSD',
                                                     Out ))
                   ]).

sos_config(Request) :-
   http_parameters(Request, [length(Length, [float])]),
   generate_random_walk_2(Length, X, Z),
   context_r_demo:construct_psd(X, Z, '"X, Z\\n"', Out),
   reply_html_page(% cliopatria(default),
                   [title('Superposition of sines from semi-Markov PSD'),
                    \(con_text:style_cliopatria)
                    % \(context_graphing:dygraph_script_load)
                   ],
                   [
                    \(context_graphing:dygraph_plot(false,
                                                    '',
                                                    'x',
                                                    'z',
                             'Superposition of sines from semi-Markov PSD',
                                                    Out ))
                   ]).

