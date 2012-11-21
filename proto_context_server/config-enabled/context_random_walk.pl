:- module(context_random_walk, [semi_random_walker/6]).

/** <module> Random walk algorithms
    * Terrain Profiles
    * Smi-Markov
*/

:- use_module(context_math).

% :- context:register(context_random_walk:navigate).
:- context:register(context_random_walk:demo).
:- context:register(context_random_walk:demo_ou).
:- context:register(context_random_walk:semi).
:- context:register(context_random_walk:semi_fft).


semi_run(L, A, Value) :-
    random(R),
    Value is L - A * log(R).

flip_step(L, A, R, R, Semi, Semi, Z, Z, L, A) :-
    R < Semi, !.
flip_step([L1,L2], [A1,A2], _R, 0.0, _Semi, S, Z, Z1, [L2,L1], [A2,A1]) :-
    Z1 is -Z,
    semi_run(L1, A1, S).

semi_random_walker([], _Delta, _L, _A, _Z, _Semi, _Run, Out, Out).
semi_random_walker([_|Rest], Delta, [L1,L2], [A1,A2], Z, Semi, Run, In, Out) :-
    R is Run + Delta,
    flip_step([L1,L2], [A1,A2], R, R1, Semi, S, Z, Z1, L, A),
    semi_random_walker(Rest, Delta, L, A, Z1, S, R1, [Z1|In], Out).

semi_random_walker(X, Weight, [L1,L2], [A1,A2], Z0, Z1) :-
%    length(X, N),
%    last(X,Last),
    [First,Second|_] = X,
%    Delta is 0.5*(Second - First),
    Delta is Second - First,
%    Other_Delta is (Last-First)/N,
%    print(user_error, ['XXXXX', Delta, Other_Delta]),
    semi_run(L1, A1, Semi_Run),
    semi_random_walker(X, Delta, [L2,L1], [A2,A1], 1.0, Semi_Run, 0.0, [], Z),
    W is Weight, % * (sqrt(L1+A1)+sqrt(L2+A2)),
    ZW mapdot W .* Z,
    Z1 mapdot Z0 + ZW.

semi(_) :-
    X range [1,32768]/1,
    Z0 mapdot 0.0 .* X,
    semi_random_walker(X, 1.2, [18.0,3.0], [2.0,2.0], Z0, Z1),
    semi_random_walker(X, 0.6, [20.0,20.0], [12.0,12.0], Z1, Z2),
    semi_random_walker(X, 3.0, [20.0,20.0], [80.0,80.0], Z2, Z3),
    semi_random_walker(X, 1.0, [200.0,200.0], [800.0,800.0], Z3, Z4),
    % context_complex:tuple_list(X, Z4, [], Data),
    % convert X to real scaled value
    Data tuple X + Z4,

    reply_html_page(% cliopatria(default),
                    [title('chart'),\(con_text:style_cliopatria)],
                    [
                     \(context_graphing:dygraph_native(
                                            false,
                                            ['X','Z'],
                                            'surface distance (m)',
                                            'elevation change (m)',
                                            'random walk profile',
                                            Data ))
                    ]).

semi_fft(_) :-
    % X range [1,16384]/1,
    X range [1,32768]/1,
    Z0 mapdot 0.0 .* X,
    %                          L_up L_dn  A_up A_dn
    semi_random_walker(X, 1.2, [18.0,3.0], [2.0,2.0], Z0, Z1),
    semi_random_walker(X, 0.6, [20.0,20.0], [12.0,12.0], Z1, Z2),
    semi_random_walker(X, 3.0, [20.0,20.0], [80.0,80.0], Z2, Z3),
    semi_random_walker(X, 1.0, [200.0,200.0], [800.0,800.0], Z3, Z4),
    context_complex:fft_squared([X, Z4], Array),!,

    reply_html_page(% cliopatria(default),
                    [title('chart'),\(con_text:style_cliopatria)],
                    [
                     \(context_graphing:dygraph_native(
                                            true,
                                            ['Sx','Sz'],
                                            'wavenumber (1/m)',
                                            'PSD (m^2/m)',
                                            'Semi-Markov random walk FFT PSD',
                                            Array ))

                     /*
                     \(context_graphing:dygraph_plot(
                                            true,
                                            'Sx,Sz',
                                            'wavenumber (1/m)',
                                            'PSD (m^2/m)',
                                            'random walk FFT PSD',
                                            [XV,ZV] ))
                     */
                    ]).


% Conventional

random_walker([], _, Out, Out).
random_walker([_|Rest], Value, In, Out) :-
    random(R),
    Z is Value + R - 0.5,
    random_walker(Rest, Z, [Z|In], Out).

demo(_) :-
    X range [1,2000]/1,
    random_walker(X, 0.0, [], Z),
    reply_html_page(% cliopatria(default),
                    [title('chart'),\(con_text:style_cliopatria)],
                    [
                     \(context_graphing:dygraph_plot(
                                            false,
                                            'X,Z',
                                            'surface distance (m)',
                                            'elevation change (m)',
                                            'random walk profile',
                                            [X,Z] ))
                    ]).

% Ornstein-Uhlenbeck model

ou_random_walker(_X1, _X2, _Drag, [], _, Out, Out).
ou_random_walker(X1, X2, Drag, [_|Rest], Value, In, Out) :-
    random(R),
    (R < 0.5 ->
        Z is Value*X1 + X2
    ;
        Z is Value*X1 - X2
    ),
    ou_random_walker(X1, X2, Drag, Rest, Z, [Z|In], Out).

ou_random_walker(Hop, X, Drag, In, Out) :-
    X1 is exp(-2*Drag*X),
    X2 is sqrt(Hop*(1-exp(-2*Drag*X))/2/Drag),
    % H is sqrt(Hop),
    ou_random_walker(X1, X2, Drag, In, 0.0, [], Out).

demo_ou(_) :-
    X range [1,2000]/1,
    ou_random_walker(0.028, 1.0, 0.05, X, Z),
    reply_html_page(% cliopatria(default),
                    [title('OU chart'),\(con_text:style_cliopatria)],
                    [
                     \(context_graphing:dygraph_plot(
                                            false,
                                            'X,Z',
                                            'surface distance (m)',
                                            'elevation change (m)',
                                            'Ornstein-Uhlenbeck random walk profile',
                                            [X,Z] ))
                    ]).


