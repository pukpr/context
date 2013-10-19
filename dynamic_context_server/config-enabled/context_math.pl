:- module(context_math, [
                       op(400, yfx, .*), % real times array
                       op(500, yfx, .+), % constant add to an array
                       op(500, yfx, .-), % constant subtract to an array
                       op(500, yfx, ./), % constant divide to an array
                       op(500, yfx, ~>), % map apply
                       op(700, xfx, dot), % array evaluation
                       op(700, xfx, mapdot), % array evaluation
                       op(700, xfx, convolve), % array evaluation
                       op(700, xfx, correlate), % array evaluation
                       op(700, xfx, derivative), % array evaluation
                       op(700, xfx, integrate), % array evaluation
                       op(700, xfx, difference), % array evaluation
                       op(700, xfx, zshift), % array evaluation
                       op(700, xfx, range), % array evaluation
                       op(700, xfx, tuple), % array evaluation
                       op(700, xfx, group), % array evaluation
                       op(700, xfx, window), % array evaluation
                       op(700, xfx, expand), % array evaluation
                       op(700, xfx, shrink), % array evaluation
                       op(700, xfx, normalize), % array evaluation
                       op(700, xfx, unbias), % array evaluation
                       op(700, xfx, cat), % array evaluation
                       op(700, xfx, offset), % array evaluation
                       op(700, xfx, pdf), % array evaluation
                       op(700, xfx, ordinal), % array evaluation
                       op(700, xfx, split), % array evaluation
                       op(700, xfx, deplete), % array evaluation
                       op(700, xfx, accumulate), % array evaluation
                       op(700, xfx, lag), % array evaluation
                       dot/2,
                       mapdot/2,
                       convolve/2,
                       correlate/2,
                       derivative/2,
                       integrate/2,
                       difference/2, % same as mapdot -
                       window/2,
                       expand/2,
                       shrink/2,
                       normalize/2,
                       unbias/2,
                       cat/2,
                       offset/2,
                       zshift/2,
                       pdf/2,
                       ones/2,
                       constants/3,
		       uniform/2,
                       linear_range/3,
                       linear_fractional_range/4,
                       log_range/4,
                       range/2,
                       tuple/2,
                       group/2,
                       cumulative_histogram/3,
                       histogram/3,
                       % number_line/3,  % same as numlist
                       % sum_list/2,
                       % separate/3,
                       separate/2,
                       spatial/3,
                       index/3,
                       ordinal/2,
                       split/2,
                       deplete/2,
                       interpolate/3,
		       accumulate/2,
		       lag/2
                     ]).

/** <module>  Math operations for array manipulations
  such as dot product, convolution and functional mapping
*/


separate([], Xc, X) :- reverse(Xc, X).
separate([A|Rest], Ix, Xc) :-
    A =.. [_|X],
    separate(Rest, [X|Ix], Xc).

%%   separate(+L, -X)
%
%    Separate off functor, removes a lead functor, not needed?
%
separate(List, X) :-
    separate(List, [], X).


%%   dotproduct(+L1,+L2,+Initial_Value,-Value)
%
%   Dot product of two lists
dotproduct([],[],Final,Final).
dotproduct([YF|YR],[ZF|ZR],Initial,Final) :-
   Value is YF*ZF + Initial,
   dotproduct(YR,ZR,Value,Final).

%%   dotexpand(+X,+Y,+Initial,-Final)
%
%   Dot helper function
dotexpand([],[],Initial,Final) :-
   reverse(Initial, Final).
dotexpand([YF|YR],[ZF|ZR],Initial,Final) :-
   Value is YF*ZF,
   dotexpand(YR,ZR,[Value|Initial],Final).

%%   dotdivision(+L1,+L2,+Initial_Value,-Value)
%
%   Dot division of two lists
dotdivision([],[],Final,Final).
dotdivision([YF|YR],[ZF|ZR],Initial,Final) :-
   Value is YF/ZF + Initial,
   dotdivision(YR,ZR,Value,Final).

%%   dotexpanddiv(+X,+Y,+Initial,-Final)
%
%    Helper function
dotexpanddiv([],[],Initial,Final) :-
   reverse(Initial, Final).
dotexpanddiv([YF|YR],[ZF|ZR],Initial,Final) :-
   Value is YF/ZF,
   dotexpanddiv(YR,ZR,[Value|Initial],Final).

%%   dotexpandsum(+X,+Y,+Initial,-Final)
%
%    Helper function
dotexpandsum([],[],Initial,Final) :-
   reverse(Initial, Final).
dotexpandsum([YF|YR],[ZF|ZR],Initial,Final) :-
   Value is YF+ZF,
   dotexpandsum(YR,ZR,[Value|Initial],Final).

%%   dotexpandsubtract(+X,+Y,+Initial,-Final)
%
%    Helper function
dotexpandsubtract([],[],Initial,Final) :-
   reverse(Initial, Final).
dotexpandsubtract([YF|YR],[ZF|ZR],Initial,Final) :-
   Value is YF-ZF,
   dotexpandsubtract(YR,ZR,[Value|Initial],Final).

%%   dotscale(+Scale,+List,+Initial_Value,-Value)
%
%    Scaled unit list dot product
dotscale(_,[],Initial,Final) :-
   reverse(Initial, Final).
dotscale(A,[ZF|ZR],Initial,Final) :-
   Value is A*ZF,
   dotscale(A,ZR,[Value|Initial],Final).

%%   dotadd(+S,+Y,+Initial,-Final)
%
%    Dot add scalar to list
dotadd(_,[],Initial,Final) :-
   reverse(Initial, Final).
dotadd(A,[ZF|ZR],Initial,Final) :-
   Value is A+ZF,
   dotadd(A,ZR,[Value|Initial],Final).

%%   dotpower(+Scale,+List,+Initial_Value,-Value)
%
%    Power unit list dot product
dotpower(_,[],Initial,Final) :-
   reverse(Initial, Final).
dotpower(N,[ZF|ZR],Initial,Final) :-
   Value is ZF^N,
   dotpower(N,ZR,[Value|Initial],Final).


%%   ones_list(+X, +Scalar, +Initial, -Final)
%
%    Ones (scalar) list creator
%    Clone an array list, substituting a scalar
ones_list([], _, Final, Final).
ones_list([_|ZR], Scalar, Y, Final) :-
   !,ones_list(ZR, Scalar, [Scalar|Y], Final).

% Create an array list of scalars
ones(0, _, Final, Final).
ones(Length, Scalar, Y, Final) :-
   L is Length - 1, !,
   ones(L, Scalar, [Scalar|Y], Final).

%%   ones(+Scalar, List)
%
%    Generate a ones list

ones(Length, Final) :-
    ones(Length, 1, [], Final), !.

%%   constants(+Length, +Scalar, -List)
%
%    Generate a list of constants
constants(Length, Scalar, Final) :-
    ones(Length, Scalar, [], Final), !.
%%   uniform(+Length, -Final)
%
%    Generate a list of normalized uniform constants such that total is unity
uniform(Length, Final) :-
    Scalar is 1.0/Length,
    ones(Length, Scalar, [], Final), !.


%%   number_line(+Start, +End, +Initial, -Final)
%
%    Generate a integer numbered list,  same as *numlist*
number_line(Start, End, Final, Final) :-
   End < Start, !.
number_line(Start, End, Y, Final) :-
   L is End - 1, !,
   number_line(Start, L, [End|Y], Final).

/*
number_line(Start, End, Line) :-
    number_line(Start, End, [], Final),
    reverse(Final, Line).
*/


%%   linear_range(+Start, +End, -Range)
%
%    Generate an integer numbered list.
%    This is the same as built-in => numlist(Start, End, List)
linear_range(Start, End, Range) :-
   number_line(Start, End, [], Range).

%%   linear_fractional_range(+Start, +End, +Fraction, -Range)
%
%    Generate a fractional linear range
linear_fractional_range(Start, End, Fraction, Range) :-
   Scaled_Start is Start/Fraction,
   Scaled_End is (End-Start)/Fraction + Scaled_Start,
   number_line(Scaled_Start, Scaled_End, [], Scaled_Range),
   Range mapdot Fraction .* Scaled_Range.

%%   number_log(+Start, +End, +Proportional_Fraction, +Initial, -Final)
%
%    Generate a logarithmic range
number_log(Start, End, _, Final, Final) :-
   End < Start.
number_log(Start, End, Frac, Y, Final) :-
   Start =< End,
   L is Frac * End,
   number_log(Start, L, Frac, [End|Y], Final).

%%   log_range(+Start, +End, +Frac, -Range)
%
%    Generate a logarithmic range
log_range(Start, End, Frac, Range) :-
   number_log(Start, End, Frac, [], Range).

%%   units_apply(+List, +Decoration, +In, -Out)
%
%    Units apply to list
units_apply([], _Decoration, In, Out) :- reverse(In, Out).
units_apply([F|R], Decoration, In, Out) :-
   !, units_apply(R, Decoration, [F*Decoration|In], Out).

%%   range(+From,+To)
%
%    Range of numbers into a list
X range [Y,Z]/W*Units :-    % linear range specifier
   X1 range [Y,Z]/W,
   units_apply(X1, Units, [], X), !.

X range [Y,Z]/W :-    % linear range specifier
   Z >= Y,
   W > 0.0,
   linear_fractional_range(Y, Z, W, X), !.

X range [Y,Z]^W*Units :-    % linear range specifier
   X1 range [Y,Z]^W,
   units_apply(X1, Units, [], X), !.

X range [Y,Z]^W :-    % log range specifier
   Y > 0.0,
   Z > Y,
   (   W > 1.0 ->
       W1 is 1.0/W
   ;
       W1 = W
   ),
   log_range(Y, Z, W1, X), !.

X range W*[Y,Z]*Units :-    % linear range specifier
   X1 range W*[Y,Z],
   units_apply(X1, Units, [], X), !.

X range W*[Y,Z] :-    % range specifier
   integer(Y),
   integer(Z),
   Z > Y,
   Y1 is Z-Y+1,
   constants(Y1, W, X), !.

%%   dot(+X,+Y)
%
%    Dot product of two arrays or lists of the same length

[] dot [].
[W|X] dot [Y|Z] :-  % Simplifier
   W is Y,
   X dot Z.


X dot Y*Z :-    % Array dot product
   Y1 dot Y,
   Z1 dot Z,
   dotproduct(Y1, Z1, 0, X), !.


X dot Y/Z :-    % Array dot division
   Y1 dot Y,
   Z1 dot Z,
   dotdivision(Y1, Z1, 0, X), !.

%%   mapdot(+X,+Y)
%
%    Map Dot is a dot product of individual elements, retaining the list
%    structure.

[] mapdot [].
[W|X] mapdot [Y*Units|Z] :-  % Simplifier
   W = Y*Units,  % ----------------------------------------  watch this point
   X mapdot Z.
[W|X] mapdot [Y/Units|Z] :-  % Simplifier
   W = Y/Units,  % ----------------------------------------  watch this point
   X mapdot Z.

[W|X] mapdot [Y|Z] :-  % Simplifier
   W is Y,  % ----------------------------------------  watch this point
   X mapdot Z.

X mapdot Y^N :-    % Array mapdot product
   Y1 mapdot Y,
   integer(N),
   dotpower(N, Y1, [], X),!.

X mapdot Y*Z :-    % Array mapdot product
   Y1 mapdot Y,
   Z1 mapdot Z,
   dotexpand(Y1, Z1, [], X),!.

X mapdot Y+Z :-    % Array mapdot sum
   Y1 mapdot Y,
   Z1 mapdot Z,
   dotexpandsum(Y1, Z1, [], X),!.

X mapdot Y-Z :-    % Array mapdot subtract
   Y1 mapdot Y,
   Z1 mapdot Z,
   dotexpandsubtract(Y1, Z1, [], X),!.

% X mapdot Y :-    % Array mapdot product
%   Y =.. [C|R],
%   atom(C),
%   R1 mapdot R,
%   maplist(C, R1, X),!.

X mapdot Y/Z :-    % Array mapdot product
   Y1 mapdot Y,
   Z1 mapdot Z,
   dotexpanddiv(Y1, Z1, [], X),!.

X mapdot Y.*Z :-    % Array scalar dot product
   (
      % number(Y),
      Y1 is Y,
      Z1 mapdot Z
    ;
      Y1 is Z,
      Z1 mapdot Y
   ),
   dotscale(Y1, Z1, [], X),!.

X mapdot Y.+Z :-    % Array scalar sum adder
   (
      % number(Y),
      Y1 is Y,
      Z1 mapdot Z
    ;
      Y1 is Z,
      Z1 mapdot Y
   ),
   dotadd(Y1, Z1, [], X),!.

X mapdot Z./Y :-    % Array scalar divide product
   Y1 is 1.0/Y,
   Z1 mapdot Z,
   dotscale(Y1, Z1, [], X),!.

X mapdot Z.-Y :-    % Array scalar sum adder
   Y1 is -Y,
   Z1 mapdot Z,
   dotadd(Y1, Z1, [], X),!.

X mapdot Y~>Z :-    % Map apply
   number(Y),
   Z1 mapdot Z,
   ones_list(Z1, Y, [], X),
   !.

X mapdot Scale*Y~>Z :-    % Map apply
   number(Scale),
   Z1 mapdot Z,
   maplist(Y, Z1, X0),
   X mapdot Scale .* X0, !.


X mapdot List ~> Z :-    % Map apply
   is_list(List),
   Initial mapdot 0 .* Z,
   gather_map(List, Z, Initial, X),
   !.


X mapdot Y~>Z :-    % Map apply
   Z1 mapdot Z,
   maplist(Y, Z1, X), !.

%%   gather_map
%
%    Used by *mapdot*
gather_map([], _Z, X, X).
gather_map([Scale*Y|R], Z, Xin, Xout) :-
   X mapdot Scale*Y~>Z,
   Xnew mapdot X + Xin,
   gather_map(R, Z, Xnew, Xout).



%%   convolution(+YZ, +YI, +ZI, +Init, -Final)
%
%    List convolution
convolution([],[],_,_,Initial,Final) :-
   reverse(Initial,Final).
convolution([YF|YR],[ZF|ZR],[],[],Initial,Final) :-
   convolution(YR,ZR,[YF],[ZF],Initial,Final).
convolution([YF|YR],[ZF|ZR],YI,ZI,Initial,Final) :-
   reverse(YI,YI_R),
   dotproduct(YI_R,ZI,0.0,Value),
   convolution(YR,ZR,[YF|YI],[ZF|ZI],[Value|Initial],Final).

%%   padZeros(+La0, +Lb0, -La1, -Lb1)
%
%    Pad zeros on one array to it matches the other
%%   padZerosPrep
%
%    Used by *padZeros*
padZerosPrep([], [], L, L1) :-
   reverse(L, L1).
padZerosPrep([F1|R1], [], Initial, Final):-
   padZerosPrep(R1, [], [F1|Initial], Final).
padZerosPrep([], [_|R2], Initial, Final):-
   padZerosPrep([], R2, [0.0|Initial], Final).
padZerosPrep([F1|R1], [_|R2], Initial, Final):-
   padZerosPrep(R1, R2, [F1|Initial], Final).

padZeros(La0, Lb0, La1, Lb1):-
    padZerosPrep(La0, Lb0, [], La),
    padZerosPrep(Lb0, La0, [], Lb),
    ones_list(La, 0.0, [], Empty),
    append(La, Empty, La1),
    append(Lb, Empty, Lb1).

%%   convolve(+X,+Y)
%
%    Convolve (convulution operator) list with another list.

[] convolve [].
[W|X] convolve [Y|Z] :-  % Simplifier
   W is Y,
   X convolve Z.

X convolve Y*Z :-
   Y1 convolve Y,
   Z1 convolve Z,
   padZeros(Y1,Z1,Y2,Z2),
   convolution(Y2, Z2, [], [], [], X), !.

%%   correlate(+X,+Y)
%
%    Pair correlate one list with another list.

[] correlate [].
[W|X] correlate [Y|Z] :-  % Simplifier
   W is Y,
   X correlate Z.

X correlate Y*Z :-
   Y1 correlate Y,
   Z1 correlate Z,
   reverse(Z1, Z1_R),
   padZeros(Y1,Z1_R,Y2,Z2),
   convolution(Y2, Z2, [], [], [], X), !.

%%   change_sign(+X, -Y)
%
%    Change sign of value
change_sign(X, Y) :-
    Y is -X.

%   derive(+X,+Y,+Initial,-Derivatives)
%
%    Derivative of Y with respect to X
derive(X,Y,Initial,Derivatives) :-
    derivatives(X, Y, 0, 0, Initial, D),
    maplist(change_sign, D, Derivatives).

%%   derivative(+X,+Y)
%
%    Take the derivative of one list with respect to another list.

[] derivative [].
[W|X] derivative [Y|Z] :-  % Simplifier
   W is Y,
   X derivative Z.

X derivative Y/1 :-
   Y1 derivative Y,
   Z1 ordinal Y,
   derive(Y1,Z1,[],X), !.

X derivative Y/Z :-
   Y1 derivative Y,
   Z1 derivative Z,
   derive(Y1,Z1,[],X).

%%   accumulation(+P, +V, +LP, +LV, -Probs, -Final)
%
%    Accumulation of values, used for CDF, e.g
accumulation([], [], _, _, Probs, Final) :-
    reverse(Probs, Final).
accumulation([P|PR], [V|VR], LastP, LastV, Probs, Final) :-
    Accumul is P*(V-LastV) + LastP,
    accumulation(PR, VR, Accumul, V, [Accumul|Probs], Final).

%%   integral(+X,+Y,+Initial,-Running_Integral)
%
%    Integral of list X with delta increments in Y
integral(X,Y,Initial,Running_Integral) :-
    accumulation(X, Y, 0, 0, Initial, Running_Integral).

%%   integrate(+X,+Y)
%
%    Take the integral of one list with respect to another delta list.

[] integrate [].
[W|X] integrate [Y|Z] :-  % Simplifier
   W is Y,
   X integrate Z.

X integrate Y*Z :-
   Y1 integrate Y,
   Z1 integrate Z,
   integral(Y1,Z1,[],X).

%%   accum(+X,+Y)
%
%    Take the cumulative.

sum_list([], List, List) :- !.
sum_list([F|Rest], List, Out) :-
   sumlist(Rest, Acc),
   Sum is Acc + F,
   sum_list(Rest, [Sum|List], Out).

X accumulate Y :-
   reverse(Y, Z),
   sum_list(Z, [], X).


% W dot Y*Z :-    % Scalar*Array
%    Y1 is Y,
%   Z1 dot Z,
%   ones(Z1,1.0, [],Y2).

% Y dot [3,3,3]*[3,3,3].
%

%%   cumulative_histogram(+List, -Variates, -Probs)
%
%    Cumulative histogram of list generating Variates and Probs
cumulative_histogram(List, Variates, Probs) :-
    length(List,N),
    msort(List, Variates),
    Increment is 1.0/N,
    linear_fractional_range(Increment, 1.0, Increment, P),
    reverse(P, Probs).

%%   derivatives(+P, +V, +LP, +LV, -Probs, -Final)
%
%    Derivative of values, used for PDF,, e.g
derivatives([], [], _, _, Probs, Final) :-
    reverse(Probs, Final).
derivatives([P|PR], [V|VR], LastP, LastV, Probs, Final) :-
    (
    V = LastV -> Density = 0.0;
    Density is (LastP-P)/(V-LastV)
    ),
    derivatives(PR, VR, P, V, [Density|Probs], Final).


%%   subtract(+L1, +L1, +Initial, -Final)
%
%    Subtract two lists
subtract([], [], In, Final) :-
    reverse(In, Final).
subtract([P|PR], [V|VR], Diffs, Final) :-
    Diff is P-V,
    subtract(PR, VR, [Diff|Diffs], Final).

%%   difference(+X,+Y)
%
%    Take the difference of one list with respect to another list.

[] difference [].
[W|X] difference [Y|Z] :-  % Simplifier
   W is Y,
   X difference Z.

X difference Y-Z :-
   Y1 difference Y,
   Z1 difference Z,
   subtract(Y1,Z1,[],X).

%%   tuple_merge(+X,+Y,+Initial,-Final)
%
%    Tuple merge into a single list
tuple_merge([],_,Initial,Final) :- reverse(Initial, Final), !.
tuple_merge(_,[],Initial,Final) :- reverse(Initial, Final).
tuple_merge([Y|YR],[Z|ZR],Initial,Final) :-
   flatten([Y,Z], Tuple),
   tuple_merge(YR,ZR,[Tuple|Initial],Final).

%%   strip(+L,+Initial,-Final)
%
%    Strip units
strip([],Initial,Final) :- reverse(Initial, Final).
strip([F|R],Initial,Final) :-  %  --- remove any extra unit multipliers for the time being
   compound(F),
   F =.. [_,  Value| _],!,
   strip(R,[Value|Initial],Final).
strip([F|R],Initial,Final) :-
   !, strip(R,[F|Initial],Final).

%%   tuple(+X,+Y)
%
%    Create a tuple list from two lists.

[] tuple [].
[W|X] tuple [Y|Z] :-  % Simplifier
   number(Y),
   W is Y,
   X tuple Z.

X tuple Y+Z :-
   Y1 tuple Y,
   Z1 tuple Z,
   tuple_merge(Y1,Z1,[],X), !.

X tuple Y+Z :-
   strip(Y, [], Y1),  % found non-numbers in the array
   strip(Z, [], Z1),
   tuple_merge(Y1,Z1,[],X).


%%   convert_to_list(+X,-Y)
%
%    Convert to list
convert_to_list(X,X) :-	is_list(X), !.
convert_to_list(X,[X]).

%%   list_merge(+X,+Y,+Initial,-Final)
%
%    Tuple merge into a single list
list_merge([],_,Final,Final) :- !.
list_merge(_,[],Final,Final) :- !.
list_merge([Y|YR],[Z|ZR],Initial,Final) :-
   convert_to_list(Y,Y1),
   convert_to_list(Z,Z1),
   append(Y1,[Z1],X),
   append(Initial, [X], Next),
   list_merge(YR,ZR,Next,Final).


%%   group(+X,+Y)
%
%    Create a tuple list from two lists.

[] group [].

[Y|X] group [Y|Z] :-  % Simplifier
   % is_list(Y),
   X group Z.

X group Y+Z :-
   Y1 group Y,
   Z1 group Z,
   list_merge(Y1,Z1,[],X), !.

/*
X group Y+Z :-
   list_merge(Y,Z,[],X).

*/

%%   histogram(+List, -Variates, -Probs)
%
%    Histogram  into variates and probability
%    creating cumulative histogram array, has problems with duplicates
histogram(List, Variates, Probs) :-
    cumulative_histogram(List, Variates, CProbs),
    % CProbs = [FirstP|RestP],
    % Variates = [FirstV|RestV],
    derivatives(CProbs, Variates, 1.0, 0.0, [], Probs)
    % , remove_dupes(Probs, P)
    % , remove_dupes(Variates, V)
    .
/*
%%   list_summer(+List, +Initial, -Total)
%
%    Sum list (already built-in)
list_summer([], Total, Total).
list_summer([F|R], Total, Final) :-
    Sum is Total + F,
    list_summer(R, Sum, Final).

%%   sum_list(+List, -Sum)
%
%    Sum list (already built-in)
sum_list(List, Sum) :-
    list_summer(List, 0, Sum).   % can also use the built-in sumlist
*/

%%   unbias(+X,-Y)
%
%    Remove the mean from a list of values, so the sum is zero.

[] unbias [].
X unbias Y :-
   sumlist(Y,N),
   length(Y,L),
   Offset is -N/L,
   dotadd(Offset, Y, [], X), !.

%%   normalize(+X,-Y)
%
%    Normalize a list of values, so the sum is unity.

[] normalize [].
X normalize Y :-
   sumlist(Y,N),
   Scale is 1/N,
   dotscale(Scale, Y, [], X), !.

%%   pdf(+X,-Y)
%
%   Create a PDF from a list of values.

[] pdf [].
X pdf Y :-
   [A,B|_] = Y,
   A >= B,
   reverse(Y, Y1),
   Y2 zshift Y1 - Y1,
   X1 normalize Y2,
   reverse(X1, X),
   !.
/*
X pdf Y :-
   [A,B|_] = Y,
   A < B,
   reverse(Y,YR),
   Y1 zshift YR - YR,
   X normalize Y1,
   !.
*/

%%   zshift(+X,-Y)
%
%   Do a DSP z-shift from a list.

[] zshift [].
[W|X] zshift [Y|Z] :-
   W is Y,
   X zshift Z.

X zshift Y-Y :-
   Y1 difference Y,
   Z1 difference Y,
   append(Y1, [0], Y2),
   subtract(Y2,[0|Z1],[],X1),
   reverse(X1, [_|X2]),
   reverse(X2, X).

%%   truncate(+X, +Y, +Init, -Final)
%
%    Truncate to the smallest list
truncate(_, [], Init, Final) :- reverse(Init, Final).
truncate([L|Long], [_|Short], Init, Final) :-
   truncate(Long, Short, [L|Init], Final).

%%   sconvolution(+Y,+Window,+Initial,-Final)
%
%    Convolution against window function
sconvolution(Y,Window,Initial,Final) :-
   length(Y, LY),
   length(Window, LW),
   LY = LW,
   reverse(Initial,Final).
sconvolution([_|YR],Window,Initial,Final) :-
   truncate(YR, Window, [], YW),
   dotproduct(YW,Window,0.0,Value),
   sconvolution(YR,Window,[Value|Initial],Final).


%%   window(+X,-Y)
%
%   Do a lag-window or centered window on a list.

[] window [].
[W|X] window [Y|Z] :-  % Simplifier
   W is Y,
   X window Z.

X window Y/Z :-  % lag window
   Y1 window Y,
   Z1 window Z,
   Z2 normalize Z1,
   ones_list(Z, 0.0, [], Empty),
   append(Empty, Y1, Y2),
   sconvolution(Y2, Z2, [], X), !.

X window Y*Z :-  % centered window
   Y1 window Y,
   Z1 window Z,
   Z2 normalize Z1,
   ones_list(Z, 0.0, [], Empty),
   append(Empty, Y1, Y2),
   sconvolution(Y2, Z2, [], X0),
   length(Z1, N),
   M is integer(N/2),
   last(X0, Last),
   Last_Vals range Last*[1,M],
   % Last_Ones mapdot Last_Vals ./ N,
   X1 offset X0-M,
   MM is M + 1,
   Ramp range [MM,N]/1,
   R mapdot Ramp ./ N,
   % reverse(Ramp, R),
   length(Y1,L),
   Length is L-M,
   Ones range 1*[1,Length],
   Mult = [R|Ones], M1 cat Mult,
   X2 = [X1|Last_Vals], X3 cat X2,
   X mapdot X3 / M1,

   /*
   X2 = [X1|Zeros],
   X cat X2,
   */

   !.

/*
X window Y*Z :-  % centered window
   Y1 window Y,
   Z1 window Z,
   Z2 normalize Z1,
   X1 convolve Y1*Z2,
   length(Z2, N),
   M is integer((N+1)/2)-1,
   X2 offset X1 - M,
   X shrink X2/Y.
*/


%%   intersect(+X, +Y, +Initial, -Final)
%
%    Intersection of two lists, same as *truncate*
intersect(_, [], F, Final) :- reverse(F, Final).
intersect([F1|R1], [_|R2], Input, Final) :-
   intersect(R1, R2, [F1|Input], Final).

%%   intersect_and_pad(+L1, +L2, +Initial, -Final)
%
%    Intersect but then pad afterwards
intersect_and_pad([], L, F, Final) :-
    length(L,N),
    constants(N, 0, Append),
    reverse(F, F1),
    append(F1, Append, Final).
intersect_and_pad([F1|R1], [_|R2], Input, Final) :-
    intersect_and_pad(R1, R2, [F1|Input], Final).

%%   shrink(+X,-Y)
%
%    Shrink a list to match the second.

[] shrink [].
[W|X] shrink [Y|Z] :-  % Simplifier
   W = Y,
   X shrink Z.

X shrink Y/Z :-
   Y1 shrink Y,
   Z1 shrink Z,
   intersect(Y1, Z1, [], X), !.

%%   expand(+X,-Y)
%
%    Expand a list to match the second.

[] expand [].
[W|X] expand [Y|Z] :-  % Simplifier
   W = Y,
   X expand Z.

X expand Y+Z :-
   Y1 expand Y,
   Z1 expand Z,
   intersect_and_pad(Y1, Z1, [], X), !.

%%   cat(+X,-Y)
%
%    Flatten a list.

[] cat [].
X cat [First|Rest] :-
   flatten([First|Rest], X).

%%   reduce_by(+N, +L, -X)
%
%    Reduce list by N elements, used by *offset*
reduce_by(0, X, X).
reduce_by(N, [_|R], Out) :-
    M is N-1,
    reduce_by(M, R, Out).

%%   offset(+X,+Y)
%
%    Offset a list by N elements.

[] offset [].
X offset Y - N :-
   N > 0,
   reduce_by(N, Y, X), !.

%%   ordinal(+X,+Y)
%
%    Create an ordinal (counting) list from a list.

X ordinal Y :-
   length(Y,N),
   M is N-1,
   numlist(0, M, X).

%%   split_n(+N,+List, +Xc, -X)
%
%    Split after N value
split_n(_N, [], Xc, X) :- reverse(Xc, X).
split_n(N, [X|Rest], Ix, Xc) :-
    nth1(N, X, El),!,
    split_n(N, Rest, [El|Ix], Xc).

%%   splitter(+L, +Tuples, +Initial, -List)
%
%    Split tuples
splitter(0, _Tuples, List, List).
splitter(N, Tuples, List, Output) :-
    split_n(N, Tuples, [], FL),
    M is N-1,!,
    splitter(M, Tuples, [FL|List], Output).

splitter(Tuples, Output) :-
    is_list(Tuples),
    Tuples = [F|_],
    length(F, N),
    splitter(N, Tuples, [], Output), !.

%%   split(+X,+Y)
%
%    Split pairs of values into two sequential lists.

[] split [].
X split Y :-
   splitter(Y,X), !.



%%   index(+List, +Pos, -Index)
%
%    Index a list like an array, only on flat uniformly-space lists
index(List, Pos, Index) :-
   [First,Second|_R] = List,
   Scale is Second-First,
   Index is floor((Pos-First)/Scale+1.5).

%%   spatial(+List, +Pos, -Value)
%
%    Index a list like an array
spatial(List, Pos, Value) :-
   [First,_Second|_R] = List,
   Index is Pos-First,
   nth0(Index, List, Value).


% Flow math

%%   extract(+Cumulative, +Avail, +Extract, +Prod, -Production)
%
%    Extraction routine
extraction(_, [],_, P, Production) :- reverse(P,Production), !.
extraction(_, _,[], P, Production) :- reverse(P,Production).
extraction(Cumulative, [A|Avail], [R|Extract], Prod, Production):-
   C is Cumulative + A - R*Cumulative,
   P is R*C,
   extraction(C, Avail, Extract, [P|Prod], Production).

%%   deplete(+X,-Y)
%
%    deplete a list according to a rate.
[] deplete [].
[W|X] deplete [Y|Z] :-  % Simplifier
   W = Y,
   X deplete Z.

X deplete Y/Z :-
   Y1 deplete Y,
   Z1 deplete Z,
   extraction(0, Y1, Z1, [], X), !.


%%   interpolate(+Line,+Table,-Result)
%
%    interpolate along a number line according to a sparse table.

search_interpolate(N, [[N1,V1],[N2,V2]], Val) :-
    Val is V1 + (N-N1)*(V2-V1)/(N2-N1).
search_interpolate(N, [[N1,V1],[N2,V2]|_], Val) :-
    N >= N1, N < N2,
    Val is V1 + (N-N1)*(V2-V1)/(N2-N1).
search_interpolate(N, [[_,_],[N2,V2]|Table], Val) :-
    search_interpolate(N, [[N2,V2]|Table], Val).

interpolate([], _, Vals, Values) :- reverse(Vals, Values), !.
interpolate([N|Nums], Table, Vals, Values):-
    search_interpolate(N, Table, Val),
    !, interpolate(Nums, Table, [Val|Vals], Values).

interpolate(N, Table, Values):-
    interpolate(N, Table, [], Values).


%%   lag(+X, +A, -Y)
%
%    lag exponential smoother
%
lag([], _, _, Y_In, Y_Out) :- reverse(Y_In, Y_Out).
lag([X|X_In], A, Sum, Y_In, Y_Out) :-
    Y is (1-A)*X + A*Sum,
    lag(X_In, A, Y, [Y|Y_In], Y_Out).

lag(X, A, Y) :-
    [First|_] = X,
    X0 mapdot X .- First,
    lag(X0, A, 0.0, [], Y0),
    Y mapdot First .+ Y0.


[] lag [].
[W|X] lag [Y|Z] :-  % Simplifier
   W is Y,
   X mapdot Z.
X lag Z / Y :-
   number(Y),
   ( Y > 0 ->
     Factor is exp(-1/Y),
     lag(Z, Factor, X)
   ;
     X = Z
   ), !.
