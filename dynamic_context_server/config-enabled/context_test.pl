/** <module> Testing
    * Extra tests not aligned with main application
    *
*/

:- use_module(context_math).
:- use_module(library(clpfd)).

:- dynamic a/1.

gen_ou(D,L,N,Z) :-
   retractall(a(_)),
   NN is N*N,
   X range [1,NN]/1.0,
   D0 is D,  %% should be 65 because E-W post data is shorter
   L0 is L,
   Scale is 1.0,
   context_random_walk:ou_random_walker(D0, Scale, L0, X, Z),
   assert(a(Z)),
   !.


save_ou :-
    a(Z),tell('aaa_16_16_test.csv'),maplist(writeln,Z), told, !.

gen_ou_train :-
 N is 64*64, X range [1.0,N]/1.0, gen_ou(128, 0.0125, 64,A),
 tell('lacrosse'), writeln(X),writeln(A), told.
%
gen_semimarkov_train :-
  N is 64*64, X range [1.0,N]/1.0, Z0 mapdot 0.0 .* X,
 semi_random_walker(X, 1.0, [0.0,0.0], [71.5,71.5], Z0, A),
 tell('telegraph'), writeln(X),writeln(A), told.


:- dynamic  valuediff/3.

diff(_, _, []).
diff(N, [F1|R1], [F2|R2]) :-
	%R2 = [_|RX],
	Diff is integer(abs(F2-F1)),
	(
	valuediff(N, Diff, Val),
	 retract(valuediff(N,Diff, Val)),
	 V is Val +1,
	 assert(valuediff(N,Diff,V))
	;
	assert(valuediff(N,Diff,1))
	),
	diff(N, R1,R2).

godiff(N) :-
	a(List),
	List=[_|L1],
	diff(N, List, L1).


collect_range_info(Lat, Lon, List) :-
    Lon in -112 .. -102,
    Lat in 32 .. 49,
    label([Lon, Lat]),
    context_autocorr:collect_info(Lat, Lon, List).

print_collect_range_info :-
    collect_range_info(Lat, Lon, ['D'=Df, 'theta'=Lf, 'Q'=Quality]),
    format(atom(S), '~1f, ~1f, ~2f, ~6f, ~1f', [Lat, Lon, Df, Lf, Quality]),
    print(S), nl,
    fail.

print_collect_all_info :-
    context_autocorr:collect_info(_URI), % , ['D'=Df, 'theta'=Lf, 'Q'=Quality]),
    % format(atom(S), '~w, ~2f, ~6f, ~1f', [URI, Df, Lf, Quality]),
    % print(S), nl,
    fail.


/*
count_elevs :-
    Distance in 1..40,
    label([Distance]),
    aggregate_all(count, count_elev(Distance), N),
    print(user_error, [Distance, N]).
*/


% Yuma 11S 723568E 3619681N

% :- context:register(context_autocorr:sample_yuma).

sample_single_yuma(DeltaX, _, [Slope,Elev]) :-
    Easting = 723568,
    Northing = 3619681,
    Zone = 11,
    random(X),
    E1 is Easting + 10000 * X,
    E2 is E1 + DeltaX, % 90 meters away
    context_geo:get_elevation(E1, Northing, Zone, false, Elev1),
    context_geo:get_elevation(E2, Northing, Zone, false, Elev2),
    Slope is (Elev1-Elev2)/DeltaX,
    Elev is (Elev1+Elev2)/2.0.

% Slow Google Maps call
sample_yuma_delta_elev(DeltaX, _, DeltaZ) :-
    Easting = 723568,
    Northing = 3619681,
    Zone = 11,
    random(X),
    E1 is Easting + 10000 * X,
    E2 is E1 + DeltaX, % 90 meters away
    context_geo:get_elevation(E1, Northing, Zone, false, Elev1),
    sleep(1),
    context_geo:get_elevation(E2, Northing, Zone, false, Elev2),
    sleep(1),
    DeltaZ is (Elev2-Elev1).

sample_yuma(DeltaX, X, Y) :-
    constants(10, DeltaX, X),
    Y mapdot sample_yuma_delta_elev(DeltaX) ~> X.

iterate_yuma(DeltaX, X, Y, Xf, Yf) :-
    DeltaX =< 0.0,
    flatten(X, Xf),
    flatten(Y, Yf).
iterate_yuma(DeltaX, Xi, Yi, X, Y) :-
    sample_yuma(DeltaX, X0, Y0),
    print(user_error, [DeltaX]),
    DX is DeltaX - 90.0, !,
%    append(X0, Xi, Xn),
%    append(Y0, Yi, Yn), !,
    iterate_yuma(DX, [X0|Xi], [Y0|Yi], X, Y).


plot_yuma :-
    iterate_yuma(180.0, [],[], X, Y),
    context_r:rplot(X, Y).

% Faster Google Maps as it aggergates data
sample_yuma_delta(DeltaX, _, [E1,E2]) :-
    Easting = 723568,
    random(X),
    E1 is Easting + 10000 * X,
    random(Dir),
    (	 Dir > 0.5 ->
     E2 is E1 + DeltaX;
     E2 is E1 - DeltaX
    ).

calc_differences([], List, List).
calc_differences([X1,X2|R], Li, List) :-
    Diff is X1-X2,
    calc_differences(R, [Diff|Li], List).

sample_array_yuma(Num, DeltaX, X, Diffs) :-
    Northing = 3619681,
    Zone = 11,
    constants(Num, DeltaX, X),
    Y mapdot sample_yuma_delta(DeltaX) ~> X,
    flatten(Y, Y1),
    X1 mapdot Northing ~> Y1,
    !,
    context_geo:get_elevation([Y1,X1], Zone, false, Elev),
    calc_differences(Elev, [], Diffs),
    !.

iterate_yuma_array(_Num, DeltaX, X, Y, Xf, Yf) :-
    DeltaX =< 0.0,
    flatten(X, Xf),
    flatten(Y, Yf).
iterate_yuma_array(Num, DeltaX, Xi, Yi, X, Y) :-
    sample_array_yuma(Num, DeltaX, X0, Y0),
    print(user_error, [DeltaX]),
    sleep(1),
    DX is DeltaX - 10.0,
    !,
    iterate_yuma_array(Num, DX, [X0|Xi], [Y0|Yi], X, Y).


plot_yuma_array :-
    iterate_yuma_array(20, 2700.0, [],[], X, Y),
    % iterate_yuma_array(2, 90.0, [],[], X, Y),
    context_r:rhist2d(X, Y), !.


reversi([])    --> [].
reversi([H|T]) --> reversi(T), [H].

