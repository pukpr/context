/** <module> Unit test module
    * Uses unit test pattern framework
    * Automatically run with modifications
*/

:- use_module(context_math).
:- use_module(context_complex).
:- use_module(context_atm).
:- use_module(context_units).
:- use_module(library(clpfd)).

:- begin_tests(lists).
:- use_module(library(lists)).

within(Low, Value, High) :- Low =< Value, Value =< High, !.
near(Value,Actual) :- Actual*0.99 =< Value, Value =< Actual*1.01, !.

test(integrate_and_derivative) :-
    [1.0, 3.0, _, 10.0] integrate [10, 20, 30, 40] * [0.1, 0.2, 0.3, 0.4],
    [100.0, 100.0, _, _] derivative [10, 20, 30, 40] / [0.1, 0.2, 0.3, 0.4].  % assume first starts from zero

test(dsp) :-
    [1, 0, 0, -1, 0] zshift [1,1,1,0,0] - [1,1,1,0,0],  % detects shifts in level
    Z = [1,1,1,1,1,1,1,0,0,0,0,0,1,0],
        [1,0,0,0,0,0,0,-1 |_] zshift Z-Z,
    [1,1,1,1]  difference [4,3,2,1]-[3,2,1,0],
    [0,0,0,0,0] unbias [1,1,1,1,1],          % this is an offset normalize rather than a density normalize, removes DC spike
    [-0.5,-0.5, 0.5, 0.5] unbias [1,1,2,2].

test(list_gen) :-
    numlist(1,10,L),      % number_line(1,10,L),
    L = [1,2,3,4,5,6,7,8,9,10],
    ones(5, Ones),
    Ones = [1,1,1,1,1],
    constants(5,2, Twos),
    Twos = [2,2,2,2,2].

test(range_test) :-
    [1.0,10.0,100.0,1000.0,10000.0,100000] range [1, 100000] ^ 10,
    Y range [0.0,10.0]/0.1,
    sumlist(Y, Z),
    % sum_list(Y,Z), % this one is the same, can get rid of.
    505 is integer(Z).

test(map_and_dot) :-
    [2] mapdot [4]/[2],
    [2,4] mapdot [2,2] * [1,2],
    [20,40] mapdot  2 .* [10, 20],
    3 dot [1,1,1] * [1,1,1],
    3 dot [2,2,2] / [2,2,2].

test(mapdot_apply) :-
    [0.0,Y] mapdot sin ~> (1.0 .* [0.0, pi]),
    Y < 1.0e-8,
    [2.0, 2.0] mapdot 2 * sqrt ~> [1.0, 1.0],
    [1.0, 1.0] mapdot [0.5*sqrt, 0.5*sqrt] ~> [1.0, 1.0],
    [1,1,1] mapdot 1 ~> [2,3,4]. % Clone function, keeps same length but replaces with values

test(mapdot_associate) :-
    X range [1,10]/1,
    Y mapdot X * (X/X),
    Y = X,
    Z mapdot X-(((X+X)-X)+X - 2 .* X),
    Z = X.

test(stats) :-
    X range [0,100]/0.1,
    Y mapdot exp(10) ~> X, % generate a cumulative distribution
    Z pdf Y,
    sumlist(Z,T),
    near(T,1),
    Mean dot Z * X,  % will reproduce the mean
    near(Mean, 10).

test(tuple) :-
    Data tuple [1,2] + [20,30],
    Data = [[1, 20], [2, 30]],
    Split split [[1,3,100],[2,4,99],[7,8,88]],
    X = [1, 2, 7],
    Y = [3, 4, 8],
    Z = [100, 99, 88],
    Split = [X, Y, Z].

test(convolve_correlate) :-
    C convolve [1,1] * [1,1],
    C = [1.0,2.0,1.0],
    Y correlate [1, 2, 3, 4]*[1,2,3,4],
    Y = [4.0, 11.0, 20.0, 30.0, 20.0, 11.0, 4.0].

test(standard_atmosphere) :-
    context_atm:atmPartialPressureWater(100.0*c, Pc*atm),
    within(0.999, Pc, 1.001),
    context_atm:atmTemperatureForPartialPressureWater(1*atm, Tc*c),
    within(99.9, Tc, 100.1),
    context_atm:atmPressureDryAdiabatic(0.0*km, AtmSeaLevel*atm),
    within(0.999, AtmSeaLevel, 1.001),
    context_atm:atmAltitudeAtPressureDryAdiabatic(1.0*atm, SeaLevel*km),
    within(0.0, SeaLevel, 0.001),
    context_atm:boilingPointH20(0.0*km, Tbp*c),
    within(99.9, Tbp, 100.1).

test(standard_atmosphere_in_other_units) :-
    context_atm:atmPartialPressureWater(212.0*f, Pc*atm),
    within(0.999, Pc, 1.001),
    context_atm:atmTemperatureForPartialPressureWater(760*torr, Tc*c),
    within(99.9, Tc, 100.1),
    context_atm:atmPressureDryAdiabatic(0.0*ft, AtmSeaLevel*atm),
    within(0.999, AtmSeaLevel, 1.001),
    context_atm:atmAltitudeAtPressureDryAdiabatic(101.325*kPa, SeaLevel*km),
    within(0.0, SeaLevel, 0.001),
    context_atm:boilingPointH20(0.0*m, Tbp*c),
    within(99.9, Tbp, 100.1).

test(ideal_gas) :-
    MW=29,
    context_atm:idealDensity(MW*au, 273*k, 101.325*kpa, D*kg/m^3),
    within(1.2, D, 1.3),  % kg/m3 1.225
    context_atm:idealTemperature(MW*au, 1.294*kg/m^3, 101325*pa,T*k),
    near(T, 273.15),
    context_atm:idealPressure(MW*au, 1.2945*kg/m^3, 0*c,P*atm),
    near(P, 1.0).

test(units) :-
    context_units:convert(10*m*m*m, Vol*mm*mm*mm, Vol),  % requires exact integer relationships
    context_units:convert(10*m^2*m, Vol*mm^2*mm, Vol),
    context_units:convert(10*m^3, Vol*mm^3, Vol),
    %
    context_units:convert(1*kg/m^2, _*g/cm^2, 0.1),
    context_units:convert(1*g/in^2, X*lb/ft^2, X),
    near(X, 0.3174659),
    context_units:convert(1*ft, Y*km, Y),
    near(Y, 0.0003048),
    context_units:convert(1*in, Z*yd, Z),
    near(Z, 0.0277777),
    context_units:convert(14.696*psi, StandardAtm*atm, StandardAtm),
    near(StandardAtm, 1.0).

test(invert_units) :-
     context_units:convert(10/s/s/s, S/min/min/min, S),
     context_units:convert(10/s^3, S/min^3, S),
     context_units:convert(10/s^2/s, S/min^2/min, S).

test(mixed_units) :-
     context_units:convert(10/s/s/s*s^2*s*s*m^4/m, S/min/min/min*min^2*min*min*mm^4/mm, S),
     context_units:convert(10*s*m^3, S*min*mm^3, S),
     context_units:convert(10/s^2*s^3*m*m^2, S1/min^2*min^3*mm*mm^2, S1),
     near(S, S1).


test(units_list) :-
    List = [1*g/mm^2,
            2*g/mm^2,
            3*g/mm^2,
            4*g/mm^2],
    context_units:convert_apply(List, _*lb/m^2, [], Z),
    [A,B,_C,_D] = Z,
    near(A, 2204.62),
    near(B, 4409.25).

test(paramaterized_curve) :-
    T range [0.0, 100.0]/100.0*c,
    [P_low, P_high] mapdot atmPartialPressureWater(atm) ~> T,
    within(0.0, P_low, 0.01),
    within(0.99, P_high, 1.01),
    %
    [P1_low, P1_high] mapdot atmPartialPressureWater(torr) ~> T,
    within(0.0, P1_low, 10.0),
    within(759.0, P1_high, 761.0),
    %
    P range [0.01, 1.0]/0.01*atm,
    [D|_] mapdot idealDensity(g/cm^3,28*au,30.0*c) ~> P,
    D < 0.0001,
    %
    WL range [0.1, 20.0]/0.1*micron,
    [Pr|_] mapdot plancks_law(micron, 300*k) ~> WL,
    Pr > 0.0.



test(scientific_units) :-
    context_units:scaling(m,km,0.001),
    context_units:scaling(m,mm,1000.0),
    context_units:convert(g*hr/cm^2, kg*s/m^2, 36000.0),
    context_units:convert(5.67e-8*j/s/m^2/k^4, SB*btu/hr/ft^2/r^4, SB),
    within(0.171e-8, SB, 0.172e-8),  %% Stefan-Boltzmann in Rankine temperature
    context_units:convert(9.80665*m/s^2, G*ft/s^2, G),
    near(G, 32.174049),
    context_units:relation(fd/e, FaradaysConstant),!,
    near(FaradaysConstant, 96485.3383).

test(specification_units) :-
     context_units:convert(970*j/s/m^2, PowerDensity*btu/hr/ft^2, PowerDensity),
     near(PowerDensity,  307.0).


test(shrink_and_expand) :-  % these use other lists to adjust the list length
    [2,2] shrink [2,2,2,2]/[3,3],
    [1, 1, 1, 1, 0, 0] expand [1,1,1,1]+[2,2,2,2,2,2],
    [1, 1, 2, 2, 2, 3, 3, 3] cat [[1,1],[2,2,2],[3,3,3]], % same as flatten
    [12,13,14] offset [1,2,3,4,5,6,7,8,9,10,11,12,13,14] - 11.

test(indexing) :-
    index([10,20,30,40], 30, Index),
    Index=3,
    spatial([10,20,30,40], 10, 10), % creates a closely spaced list from longer unifom separations
    spatial([10,20,30,40], 11, 20),
    spatial([10,20,30,40], 12, 30).

test(parsing_fnctored_lists) :-
    separate([a(1,2),a(2,3),a(3,4)],A),
    A = [[1, 2], [2, 3], [3, 4]].

test(dsp) :-
    [1, 0, 0, -1, 0] zshift [1,1,1,0,0] - [1,1,1,0,0],  % detects shifts in level
    Z = [1,1,1,1,1,1,1,0,0,0,0,0,1,0],
        [1,0,0,0,0,0,0,-1 |_] zshift Z-Z,
    [1,1,1,1]  difference [4,3,2,1]-[3,2,1,0],
    [0,0,0,0,0] unbias [1,1,1,1,1],          % this is an offset normalize rather than a density normalize, removes DC spike
    [-0.5,-0.5, 0.5, 0.5] unbias [1,1,2,2].

test(fft) :-
    X range [0,100]/1,
    X1 mapdot sin ~> X,
    Y fft X1,
    nth0(20, Y, Peak), % This is 128/(2*pi) which is the fundamental period of a sin wave.
    nth0(5, Y, Noise),
    Peak > 10,
    Noise < 1.

test(complex_math) :-
    real(5&7, 5),
    imag(5&7, 7),
    2&0 isx (1&1) * (1&(-1)),
    2&0 isx 1&1 * 1&(-1),
    2&2 isx 1&1 + 1&1,
    2.0&0.0 isx (2&2) / (1&1),
    [1&0.0, 1&0.0, 2&0.0] mapx [1,1,2],  % list constructor for complex numbers
    0&0 isx 10&10 - 10&10,
    10&20 isx 5 &* 2&4,
    1.0&0.0 isx ((1&1) + (1&1))/((1&1) + (1&1)).

test(water_density) :-
    Input = '20*c',
    Units = 'g/cm^3',
    Internal = 'g/dm^3',
    atom_to_term(Input, I, _I),
    context_units:convert(I, To*c, To),
    context_water:alg_density(To, 0.0, R),
    atom_to_term(Units, U, _U),
    atom_to_term(Internal, At, _At),
    context_units:convert(R*At, Result*U, Result),
    near(Result, 1.0).

test(json) :-
    atom_json_term('{"a":"b"}', JSO, []),
    JSO = json([a=b]),
    atom_json_term(T, json([a=b]), [as(atom)]),
    T ='{"a":"b"}' .

test(sweet) :-
    findall(Out,
            context_sweet_utils:find_sweet_pairs([_Name,Out]),
            L),
    length(L, N),
    N>10.

test(rdf) :-
   context:create_global_term('ent:model', Term),
   rdf(A, Term,      B),
   rdf(A, ent:model, B),
   context:concise_term(ent:model, 'ent:model'),
   !.


test(context_ontology_not_loaded) :-
        rdf(_Subject, dc:title, _Object),
        !.

test(dbpedia) :-
    context_temperature:strip_numbers([literal(type(xsd:int, '29')),
                                       literal(type(xsd:int, '31'))], [], [29,31]),
    context_temperature:process_location('Baltimore',
                                         cold(MinCold, MaxCold, DayCold),
                                         hot(MinHot, MaxHot, DayHot),
                                         _Lows,
                                         _Highs,
                                         Name),
    MinCold = 29,
    MaxCold = 44,
    DayCold = 15,
    MinHot = 73,
    MaxHot = 91,
    DayHot = 195,
    Name = 'Baltimore'.

:- end_tests(lists).


