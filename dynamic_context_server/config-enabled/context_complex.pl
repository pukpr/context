:- module(context_complex, [
                            op(100, xfx, &),   % complex number constructor
                            op(400, yfx, &*),  % real times complex
                            op(700, xfx, isx), % complex arithmetic evaluation
                            op(700, xfx, mapx), % complex list constructor
                            op(700, xfx, fft),
                            isx/2,
                            mapx/2,
                            real/2,
                            imag/2,
                            fft/2 % fft/3
                           ]
         ).

/** <module>  Math operations for complex numbers
  * multiply
  * divide
  * FFT
*/

/*
:- op(100, xfx, &). % complex number constructor
:- op(400, yfx, &*). % real times complex
:- op(700, xfx, isx). % complex arithmetic evaluation

% :- op(500, yfx, &+). % complex add
% :- op(500, yfx, &-). % complex subtract
% :- op(400, yfx, &/). % complex divide
*/

:- use_module(context_math).



%%   real(+Complex, -Real)
%
%    Pull out the real part of a complex number
real(R&_, R).


%%   imag(+Complex, -Imag)
%
%    Pull out the imaginary part of a complex number
imag(_&I, I).

%%   isx(+X, Y)
%
%    Create a complex number

W&X isx Y&Z :-
    X is Z,
    W is Y. % Simplifying complex number

W&X isx Y+Z :-    % Complex add
    Y1 isx Y,
    Z1 isx Z,
    real(Y1, RY1),
    real(Z1, RZ1),
    W is RY1 + RZ1,
    imag(Y1, IY1),
    imag(Z1, IZ1),
    X is IY1 + IZ1, !.

W&X isx Y-Z :-    % Complex subtract
    Y1 isx Y,
    Z1 isx Z,
    real(Y1, RY1),
    real(Z1, RZ1),
    W is RY1 - RZ1,
    imag(Y1, IY1),
    imag(Z1, IZ1),
    X is IY1 - IZ1, !.

V&W isx X &* Y&Z :- % multiply Real times Complex
    V is X * Y,
    W is X * Z, !.

W&X isx Y*Z :-    % Complex multiply
    Y1 isx Y,
    Z1 isx Z,
    real(Y1, RY1),
    real(Z1, RZ1),
    imag(Y1, IY1),
    imag(Z1, IZ1),
    W is RY1 * RZ1 - IY1 * IZ1,
    X is IY1 * RZ1 + IZ1 * RY1, !.

W&X isx Y/Z :-    % Complex divide
    Y1 isx Y,
    Z1 isx Z,
    real(Y1, RY1),
    real(Z1, RZ1),
    imag(Y1, IY1),
    imag(Z1, IZ1),
    AmpY is sqrt(RY1*RY1 + IY1*IY1),
    PhiY is atan(IY1, RY1),
    AmpZ is sqrt(RZ1*RZ1 + IZ1*IZ1),
    PhiZ is atan(IZ1, RZ1),
    W is AmpY/AmpZ*cos(PhiY-PhiZ),
    X is AmpY/AmpZ*sin(PhiY-PhiZ), !.


W&X isx Y^N :-    % Complex power
    Y1 isx Y,
    N1 is N,
    real(Y1, RY1),
    imag(Y1, IY1),
    AmpY is sqrt(RY1*RY1 + IY1*IY1),
    PhiY is atan(IY1, RY1),
    W is (AmpY^N1)*cos(PhiY*N1),
    X is (AmpY^N1)*sin(PhiY*N1), !.

% want constructors here too
%  complex(modulus=1.0, argument=Theta)
%  complex(real=Alpha, imaginary=S)

W&X isx A*exp(i*B) :-    % Constructor
    W is A*cos(B),
    X is A*sin(B), !.

%%   mapx(+X, Y)
%
%    Create a complex list

[] mapx [].
[W|X] mapx [Y|Z] :-  % List constructor
   W = Y&0.0,
   X mapx Z, !.

% Faster versions, for FFT, etc.

% sum(A, B, C) is true if C is the sum of the complex numbers A and B.
%%   sum(+C1, +C2, -C3)
%
%    Sum of two complex numbers
sum(Ra&Ia, Rb&Ib, Rc&Ic) :-
    Rc is Ra + Rb,
    Ic is Ia + Ib.

% product(A, B, C) is true if C is the product of the complex numbers A and B.
%%   product(+C1, +C2, -C3)
%
%    Product of two complex numbers
product(Ra&Ia, Rb&Ib, Rc&Ic) :-
    Rc is Ra*Rb - Ia*Ib,
    Ic is Ra*Ib + Ia*Rb.

% list operations
%

%%   magnitude(+Array, +L, +Count, +Initial, -Final)
%
%    Calculate conjugate-squared magnitude of complex array
magnitude(_, L, L, Final, F) :- reverse(Final, F).
magnitude([Real&Im|Rest], L, Count, Initial, Final) :-
   Mag is Real*Real + Im*Im,
   C is Count + 1,
   magnitude(Rest, L, C, [Mag|Initial], Final).
/*
magnitude([], Final, F) :- reverse(Final, F).
magnitude([Real&Im|Rest], Initial, Final) :-
   Mag is Real*Real + Im*Im,
   magnitude(Rest, [Mag|Initial], Final).
*/
%%   absolute(List,Final, F)
%
%    Calculate absolute valute
absolute([],Final, F) :- reverse(Final, F).
absolute([Real&Im|Rest], Initial, Final) :-
   Abs is sqrt(Real*Real + Im*Im),
   absolute(Rest, [Abs|Initial], Final).

%%   reciprocal(+List, +L, +C, +X, +Sx, +Init, -Recip)
%
%    Gnerate a reciprocal space
reciprocal(_, L, L, _, _, Final, F) :- reverse(Final, F).
reciprocal([_|Rest], L, Count, Xm, Sx, Initial, Final) :-
   S is 2*pi/Xm + Sx,
   C is Count + 1,
   reciprocal(Rest, L, C, Xm, S, [S|Initial], Final).
/*
reciprocal(_, _, _, Final, F) :- reverse(Final, F).
reciprocal([_|Rest], Xm, Sx, Initial, Final) :-
   S is 2*pi/Xm + Sx,
   reciprocal(Rest, Xm, S, [S|Initial], Final).
*/
%%   construct(+Reals, +Init, -X)
%
%    Construct a list of complex from reals
construct([], Xc, X) :- reverse(Xc, X).
construct([X|Rest], Init, Xc) :-
    construct(Rest, [X&0.0|Init], Xc).

%%   parse(+Tuple, +Xc, -X, +Yc, -Y)
%
%    Parse out tuple list into X and Y
parse([], Xc, X, Yc, Y) :- reverse(Xc, X), reverse(Yc, Y).
% parse([xy(X,Y)|Rest], Ix, Xc, Iy, Yc) :-
%    parse(Rest, [X|Ix], Xc, [Y|Iy], Yc).
parse([[X,Y|_]|Rest], Ix, Xc, Iy, Yc) :-
    parse(Rest, [X|Ix], Xc, [Y|Iy], Yc).

%%   tuple_list(+X, +Y, +Initial, -Y)
%
%    Construct XY tuples out of X and Y list
tuple_list([], [], Initial, Final) :- reverse(Initial, Final).
tuple_list([X|XR], [Y|YR], Initial, Final) :-
    tuple_list(XR, YR, [[X,Y]|Initial], Final).

% Tests
% -----------
%	 A&B isx 1&2 * 2&1.
%   A&B isx ((1&1) + (1&1))/((1&1) + (1&1))


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The (Cooley-Tukey) Algorithm - recursive, 1-d, unordered radix 2 fft
%
% fft(N, F, Ft) is true if the list Ft contains the Fourier transform of
% the N -- a power of two -- samples in the list F.  Each sample is a
% complex number represented by c(RealPart, ImaginaryPart).

%%   fft(+N, +Initial, -Ft)
%
%    Fast Fourier Transform
fft(1, Ft, Ft) :- !.
fft(N, F, Ft) :-
    N > 1,
    N2 is N // 2,
    evens_and_odds(F, E, O),
    fft(N2, E, Et),
    fft(N2, O, Ot),
    w(1, W1),
    w(2, W2),
    w(N, Wn),
    product_and_sum(Et, Ot, W2, Wn, Gt, []),
    product_and_sum(Et, Ot, W1, Wn, Ft, Gt).

% Multiply and Add vectors
%%   product_and_sum(+L1, +L2, +W1, +W2, -F1, -F2)
%
%    Used by *fft*
product_and_sum([], [], _, _, Ft, Ft).
product_and_sum([E| Et], [O| Ot], Wk, Wn, [F| Ft], Fu) :-
    /* these are slower
      Temp isx O * Wk,
      F isx E + Temp,
      Wk1 isx Wk * Wn,
    */
    product(O, Wk, Temp),
    sum(E, Temp, F),
    product(Wk, Wn, Wk1),
    product_and_sum(Et, Ot, Wk1, Wn, Ft, Fu).

%   evens_and_odds(Xs, Evens, Odds) is true if Evens is the list of the
%   even-positioned elements of the list Xs, and Odds is the list of the
%   odd-positioned elements of the list Xs, where the first element of Xs
%   is considered to be at an even position.

%%   evens_and_odds(+A, +B, +C)
%
%    Used by *fft*
evens_and_odds([], [], []).
evens_and_odds([X| Xs], [X| Ys], Zs) :-
    evens_and_odds(Xs, Zs, Ys).


%%   w(+A, +B)
%
%    Used by *fft*
w(     1, 1.0 & 0.0).
w(     2, -1.0 & 0.0).
w(     4, 0.0 & 1.0).
w(     8, 0.707106781186548 & 0.707106781186547).
w(    16, 0.923879532511287 & 0.382683432365090).
w(    32, 0.980785280403230 & 0.195090322016128).
w(    64, 0.995184726672197 & 0.098017140329561).
w(   128, 0.998795456205172 & 0.049067674327418).
w(   256, 0.999698818696204 & 0.024541228522912).
w(   512, 0.999924701839145 & 0.012271538285720).
w(  1024, 0.999981175282601 & 0.006135884649154).
w(  2048, 0.999995293809576 & 0.003067956762966).
w(  4096, 0.999998823451702 & 0.001533980186285).
w(  8192, 0.999999705862882 & 0.000766990318743).
w( 16384, 0.999999926465718 & 0.000383495187571).
w( 32768, 0.999999981616429 & 0.000191747597311).
w( 65536, 0.9999999954041073 & 0.0000958737990959773).
w(131072, 0.999999998851027 & 0.000047936899603).
w(262144, 0.999999999712757 & 0.000023968449808).
w(524288, 0.999999999928189 & 0.000011984224905).


%%   scale_down(+L, +Scale, +Xc, -X)
%
%    Scale elements in a list down by a factor
scale_down([], _S, Xc, X) :- reverse(Xc, X).
scale_down([X|Rest], S, Init, Xc) :-
    XX is X/S,
    scale_down(Rest, S, [XX|Init], Xc).

%%   fft_squared(+X,+Y, -Sx, -Sy)
%
%    FFT mag squared
fft_squared(X,Y, Sx, Sy) :-
    sumlist(Y, Total),
    length(Y,Tn),
    Offset is -Total/Tn,
    Y0 mapdot Offset .+ Y,
    length(X, N),
    w(NN, _),
    NN >= N, !,
    Extra is NN - N,
    context_math:constants(Extra, 0.0, Zeros),
    append(X, Zeros, XX),  % should be append?
    append(Y0, Zeros, YY),
    L is NN/2,
    construct(YY, [], Yc),
    % last(XX, XL),
    [First,Second|_] = X,
    XL is (Second-First)*NN,
    print(user_error, ['FFT LENGTH', XL]),
    reciprocal(XX, L, 0, XL, 0.0, [], [_|Sx]),
    print(user_error, fft(NN)),
    fft(NN, Yc, Fy),
    magnitude(Fy, L, 0, [], SS), % combine magnitude and scale down
    Scale is NN, % (NN/2/pi)^2,
    scale_down(SS, Scale, [], [_|Sy]), % combine with above
    print(user_error, 'done').

fft_squared(List, Sx, Sy) :-
    parse(List, [], X, [], Y),
    fft_squared(X,Y, Sx, Sy),!.

fft_squared([X,Y], List) :-
    fft_squared(X,Y, Sx, Sy),
    tuple_list(Sx,Sy, [], List),!.

%%   fft_length(+Input, -Length)
%
%    Returns the size of an FFT comp
fft_length(Input, Length) :-
    length(Input, L),
    w(Length, _),
    Length >= L, !.

%%   fft_pad(+Input, -Padded)
%
%    Pad an array to return a 2^n FFT size
fft_pad(Input, Padded) :-
    fft_length(Input, Length),
    length(Input, L),
    Extra is Length - L,
    context_math:constants(Extra, 0.0, Zeros),
    append(Input, Zeros, Padded).


%%   fft(+X, +Y)
%
%    Fast Fourier Transform

Y fft X :-  % assumes a real array coming in
    length(X, N),
    w(NN, _),
    NN >= N, !,
    Extra is NN - N,
    context_math:constants(Extra, 0.0, Zeros),
    append(X, Zeros, X1),
    L is NN/2,
    construct(X1, [], X2),  % same as mapx
    fft(NN, X2, Y0),
    magnitude(Y0, L, 0, [], YM), % combine magnitude and scale down
    Scale is NN,
    scale_down(YM, Scale, [], Y), !.



% context_complex:fft_squared([xy(1.0,1.0), xy(2.0, 1.0), xy(3.0, 1.0),
% xy(4.0,0.0)], Sx, Sy).


