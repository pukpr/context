:- module(context_units, [
                          collect_unit_options/2,
                          scaling/3
                         ]).

/** <module> Units module
    * Symbolic dimensional analysis
    *
*/

alias(mmhg, torr).
alias(cc, ml).
alias(l, cubicdecimeter). % dm^3

% Quantity
relation(number/mol, 6.02214129e23).  % Avogadro's constant

% Distance -- English
relation(in/ft, 12.0).
relation(ft/yd, 3.0).
relation(ft/kft, 1000.0).
relation(yd/mi, 1760).
relation(mil/in, 1000.0).
% Distance -- Metric
relation(angstrom/nm, 10.0).
relation(nm/micron, 1000.0).
relation(micron/mm, 1000.0).
relation(mm/cm, 10.0).
relation(cm/dm, 10.0).
relation(dm/m, 10.0).
relation(m/km, 1000.0).

% Distance -- English to Metric
relation(ft/m, 3.28084).

% Mass -- English
relation(oz/lb, 16.0).
relation(lb/ton, 2000.0).
relation(lb/slug, 32.17405).
% Mass -- Metric
relation(mg/g, 1000.0).
relation(g/kg, 1000.0).
% Mass -- English to Metric
relation(g/lb, 453.592).
% Atomic units
relation(g/au, Mass) :- relation(number/mol, N), Mass is 1.0/N.

% Time
relation(ns/mic, 1000.0).
relation(mics/ms, 1000.0).
relation(ms/s, 1000.0).
relation(s/min, 60.0).
relation(min/hr, 60.0).
relation(hr/day, 24.0).
relation(day/yr, 365.0).
relation(yr/decade, 10.0).
relation(decade/century, 10.0).

% Volume - Metric
relation(ml/l, 1000.0).
% Volume - English
relation(oz/gal, 128.0).
% Volume - English to Metric
relation(l/gal, 3.78541).

% Area - English to Metric
relation(hectare/acre, 2.47105).

% Voltage
relation(v/kv, 1000.0).

% Current
relation(milliamp/amp, 1000.0).
relation(amp/kamp, 1000.0).

% Electrical charge
relation(coulombs/e, 1.60217653e-19).
relation(fd/e, V) :- % Farad
	relation(coulombs/e, E), relation(number/mol, N), V is E*N.

% Force
relation(n/lb, 0.2248).

% Power in watts
relation(w/kw, 1000.0).
relation(kw/mw, 1000.0).

% Various energy units
relation(erg/j, 10000000).
relation(j/wh, 3600).
relation(j/cal, 4.184).
relation(j/btu, 1055.05585).
relation(j/ev, 1.60217646e-19).

% Pressure
relation(mtorr/torr, 1000.0).
relation(torr/atm, 760.0).
relation(mpa/pa, 1000.0).
relation(pa/atm, 101325.0).
relation(kpa/pa, 0.001).
relation(kpa/pa, 0.001).
relation(pa/psi, 6894.75729).
relation(torr/millibar, 0.750061683).
relation(millibar/bar, 1000.0).

% temperature
relation(r/k, 1.8). % rankine temperature
relation(k/c, 1.0). % only for relative
relation(f/c, 1.8).

% Temporal Frequency
relation(hz/khz, 1000.0).

% Trig
relation(mrads/rads, 1000.0).
relation(rads/cycle, Pi2) :- Pi2 is 2*pi.
relation(deg/rads, D) :- D is 180/(2*pi).

% Multipliers
relation(1/kilo, 1000.0).
relation(kilo/mega, 10000.0).

% Miscellaneous for particle counting, etc
relation(ppm/percent, 10000).
relation(percent/fraction, 100).



check_intermediate_length(List) :-
    length(List,N),
    N > 5,
    print(user_error, ['not converging']),
    throw(error(syntax_error('not converging'))).
check_intermediate_length(_List).


scale(From, To, X, X, _List) :-
    (	alias(From, To);
        alias(To, From)), !.
scale(From, To, X, Y, _List) :-
    relation(From/To, Factor),
    Y is X * Factor, !.
scale(From, To, X, Y, _List) :-
    relation(To/From, Factor),
    Y is X/Factor, !.
scale(From, To, X, Y, List) :-
    relation(From/Intermediate, Factor),
    not(member(Intermediate, List)), % necessary to eliminate loops
    Z is X * Factor,
    scale(Intermediate, To, Z, Y, [Intermediate|List]).
scale(From, To, X, Y, List) :-
    relation(Intermediate/From, Factor),
    not(member(Intermediate, List)), % necessary to eliminate loops
    Z is X / Factor,
    scale(Intermediate, To, Z, Y, [Intermediate|List]).

% convert(_, _, _, error, error).
convert([], [], _, Final, Final) :- !.
convert(['*' |Fr], ['*'|Tr], _, Value, Final) :-
    !,
    convert(Fr, Tr, '*', Value, Final).
convert(['/' |Fr], ['/'|Tr], _, Value, Final) :-
    !,
    convert(Fr, Tr, '/', Value, Final).

convert(['^', From, Exp], ['^', To, Exp], '/', Value, Final) :-
    scale(From, To, 1.0, S, []),
    !,
    Final is Value * (S ^ Exp).
convert(['^', From, Exp], ['^', To, Exp], '*', Value, Final) :-
    scale(To, From, 1.0, S, []),
    !,
    Final is Value * (S ^ Exp).

convert([From |Fr], [_|Tr], '*', Value, Final) :-
    number(From),
    S is Value * From,
    !,
    convert(Fr, Tr, '*', S, Final).
convert([From |Fr], [_|Tr], '/', Value, Final) :-
    number(From),
    S is Value * From,      % -------------------------------------------------------
    !,
    convert(Fr, Tr, '/', S, Final).

%%
convert([From|_], [To|_], _, _Value, Final) :-
    atom(From),
    atom(To),
    check_relations(From,To, Final).

convert([From|Fr], [To|Tr], '/', Value, Final) :-
    atom(From),
    atom(To),
    scale(From, To, Value, S, []),
    !,
    convert(Fr, Tr, '/', S, Final).
convert([From|Fr], [To|Tr], '*', Value, Final) :-
    atom(From),
    atom(To),
    scale(To, From, Value, S, []),
    !,
    convert(Fr, Tr, '*', S, Final).

convert([From|Fr], [To|Tr], M, Value, Final) :-
    From =.. FromList,
    To =.. ToList,
    !,
    convert(FromList, ToList, M, Value, F),
    convert(Fr, Tr, M, F, Final).


% identical source and destination
convert(From*X, From*X, From) :- !.
% convert(From/X, From/X, From) :- !.


% Temperature is special case
convert(From*c, To*f, To) :-
    To is From*9/5 + 32, !.
convert(From*f, To*c, To) :-
    To is (From-32)*5/9, !.
convert(From*c, To*k, To) :-
    To is From + 273.15, !.
convert(From*k, To*c, To) :-
    To is From - 273.15, !.
convert(From*k, To*f, To) :-
    To is (From-273.15)*9/5 + 32, !.
convert(From*f, To*k, To) :-
    To is (From-32)*5/9 + 273.15, !.
% need rankine

% The rest are scalar multipliers


convert(From, To, Final) :-
    From =.. FromList,
    To =.. ToList,
    lower_list(FromList, [], FL),
    lower_list(ToList, [], TL),
    check_lengths(FL, TL),
    convert(FL, TL, _, 1.0, Final),
    !.


convert_apply([], _, Input, Output) :- reverse(Input, Output).
convert_apply([F|R], To, Input, Final) :-
    convert(F, To, A),
    convert_apply(R, To, [A|Input], Final).

%    (   is_list(F) ->

/*
convert_apply([], _, Input, Output) :- reverse(Input, Output).
convert_apply([F|R], To, Input, Output) :-
    convert(F, T*To, T),
    convert_apply(R, To, [T|Input], Output).
*/
/*
convert([F|L], To, Output) :-
    print(user_error, [F]),
    convert_apply([F|L], To, [], Output).
*/


check_lengths(From, To) :-
    findall(K1,sub_term(K1,From), F),
    findall(K2,sub_term(K2,To), T),
    same_length(F,T).
    % length(F, N),
    % length(T, N).
check_lengths(_, _) :-
    print(user_error, 'expressions must have same number of unit terms'),
    fail.

    %       alias(LC, Alias)

lower_list([], In, Out) :- reverse(In,Out). %, flatten(R, Out).
lower_list([F|R], In, Out) :-
    (	var(F); number(F) ),
    lower_list(R, [F|In], Out).
lower_list([F|R], In, Out) :-
    term_to_atom(F, Atom),
    downcase_atom(Atom, LC),
    term_to_atom(Alias, LC),
    lower_list(R, [Alias|In], Out).



:- dynamic relations/1.

init_relations :-
   retractall(relations(_)),
   findall([X,Y], (relation(X/Y,_);
		  alias(X,Y)), List),
   flatten(List, L),
   assert(relations(L)).

:- init_relations.

check_relations(X, Y, 'NaN') :-
    relations(L),
    (   not(member(X, L)),
        print(user_error, [X = ' not valid unit']),
        throw(error(syntax_error(X)))
    ;
        not(member(Y,L)),
        print(user_error, [Y = ' not valid unit']),
        throw(error(syntax_error(Y)))
    ).

% calculate(Op, Inputs, Outputs) :-   call(Op, Inputs, Outputs).


%     ---------------------------------------------------


collect_unit_options(Functor, List) :-
    findall(option([value(Value)],[Name]),
            (   rdfS(UID, ent:name, Functor),
	        rdf(UID, ent:units, Unit),
                rdfS(Unit, ent:unit, Value),
                rdfS(Unit, ent:description, Name)
            ),
            List).

scaling(From, To, Scale) :-
    context_units:convert(1*From, Scale*To, Scale).
