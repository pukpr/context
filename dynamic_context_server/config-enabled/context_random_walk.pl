:- module(context_random_walk, [semi_random_walker/6,
			        two_level_random_walk/4]
	 ).

/** <module> Random walk algorithms
    * Terrain Profiles
    * Semi-Markov
    * Random Walk
    * Ornstein-Uhlenbeck Random Walk
*/

:- use_module(context_math).

% semi-Markov random walk

%%   semi_run(+L, +A, +Value)
%
%    A semiMarkov run length
semi_run(L, A, Value) :-
    random(R),
    Value is L - A * log(R).

%%   flip_step(+L_in, +A, +R_in, +R, +Semi_in, +Semi, +Z_in, +Z, +L, -Flip)
%
%    Whether to flip a step
flip_step(L, A, R, R, Semi, Semi, Z, Z, L, A) :-
    R < Semi, !.
flip_step([L1,L2], [A1,A2], _R, 0.0, _Semi, S, Z, Z1, [L2,L1], [A2,A1]) :-
    Z1 is -Z,
    semi_run(L1, A1, S).

%%   semi_random_walker(+X, +Weight, +L, +A, +In, -Out)
%
%    Semi-Markov random walker
semi_random_walker([], _Delta, _L, _A, _Z, _Semi, _Run, Out, Out).
semi_random_walker([_|Rest], Delta, [L1,L2], [A1,A2], Z, Semi, Run, In, Out) :-
    R is Run + Delta,
    flip_step([L1,L2], [A1,A2], R, R1, Semi, S, Z, Z1, L, A),
    semi_random_walker(Rest, Delta, L, A, Z1, S, R1, [Z1|In], Out).

semi_random_walker(X, Weight, [L1,L2], [A1,A2], Z0, Z1) :-
    [First,Second|_] = X,
    Delta is Second - First,
    semi_run(L1, A1, Semi_Run),
    semi_random_walker(X, Delta, [L2,L1], [A2,A1], 1.0, Semi_Run, 0.0, [], Z),
    W is Weight, % * (sqrt(L1+A1)+sqrt(L2+A2)),
    ZW mapdot W .* Z,
    Z1 mapdot Z0 + ZW.


%%   two_level_random_walk(+Course_Name, +X, +Z, -Result)
%
%    Semi-Markov random walker
two_level_random_walk(Course_Name, X, Z, Result) :-  % rename this
    rdfS(Course, ent:name, Course_Name),
    rdfR(Course, ent:alpha1, Alpha1),
    rdfR(Course, ent:l1, L1),
    rdfR(Course, ent:alpha2, Alpha2),
    rdfR(Course, ent:l2, L2),
    rdfR(Course, ent:weight, Weight),
    semi_random_walker(X, Weight, [L1,L2], [Alpha1,Alpha2], Z, Result).

two_level_random_walk(Course_Name, X, _Z, Result) :-  % this does not accumulate
    rdfS(Course, ent:name, Course_Name),
    rdfR(Course, ent:diffusion, Diffusion),
    rdfR(Course, ent:drag, Drag),
    rdfR(Course, ent:spacing, DX),
    rdfR(Course, ent:weight, _Weight),
    ou_random_walker(Diffusion, DX, Drag, X, Result).

% Conventional random walk
%
%%   random_walker(+X, +Value, +In, -Out)
%
%    Classical random walk
random_walker([], _, Out, Out).
random_walker([_|Rest], Value, In, Out) :-
    random(R),
    Z is Value + R - 0.5,
    random_walker(Rest, Z, [Z|In], Out).


% Ornstein-Uhlenbeck model
%
%%   ou_random_walker(+X1, +X2, +Drag, +Y, +Value, +N, -Out)
%
%    Ornstein-Uhlenbeck random walk
ou_random_walker(_X1, _X2, _Drag, [], _, O, Out) :- reverse(O, Out).
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
    ou_random_walker(X1, X2, Drag, In, 0.0, [], Out).


