:- module(context_r_demo, []).

/** <module> Interfacing with R
    * Bessel function
    * Comparison to other math
*/

:- use_module(context_complex).
:- use_module(library(real)).
:- use_module(context_math).

:- op(400, xfx, ~).

% Note that [with(non_interactive)] is required for Linux but not Windows

:- context:register(context_r_demo:r_app).

%%   xy(+L, -X, -Y)
%
%    Tuple untangle
xy([], Xc, X, Yc, Y) :- reverse(Xc, X), reverse(Yc, Y).
xy([[X,Y]|Rest], Ix, Xc, Iy, Yc) :-
    xy(Rest, [X|Ix], Xc, [Y|Iy], Yc).

xy(Pairs, X, Y) :-
    xy(Pairs, [], X, [], Y).

%%   xyz(+L, -X, -Y, -Z)
%
%    Tuple untangle
xyz([], Xc, X, Yc, Y, Zc, Z) :- reverse(Xc, X), reverse(Yc, Y), reverse(Zc, Z).
xyz([[X,Y,Z]|Rest], Ix, Xc, Iy, Yc, Iz, Zc) :-
    xyz(Rest, [X|Ix], Xc, [Y|Iy], Yc, [Z|Iz], Zc).

xyz(Triples, X, Y, Z) :-
    xyz(Triples, [], X, [], Y, [], Z).


%%   r_open_session
%
%    Open R session
r_open_session :-
    % getenv('OS', 'Windows_NT'),
    current_prolog_flag(windows, true),
    true. % r_open([]), !.
r_open_session :-
    true. % r_open([with(non_interactive)]).

%%   open_mat(N, Lat, Lon)
%
%    Open Matlab file in R
open_mat(N, Lat, Lon) :-
     r_open_session,
     % r_in(
	 <- library('R.matlab'),
     % ),
     % r_in(
	 <- library('R.utils'),
     % ),
     data <- readMat('"./Rough_Track_Rock_Bed.mat"'),
     y <- 'data$Y[0:1024]',
     z <- 'data$Z[0:1024]',
     lat <- 'data$Start.Coordinate[1]',
     lon <- 'data$Start.Coordinate[2]',
     Lat <- lat,
     Lon <- lon,
     Y <- y,
     Z <- z,
     r_close,!,
     %,
     length(Z, N),
     context_r:rplot(Y,Z)
     .

%%   rtest
%
%    R test
rtest :-
     r_open_session,
     y <- rnorm(50),
     %REAL r_print( y ),
     x <- rnorm(y),
     %r_in(
       <- x11(width=5,height=3.5),
     %),
     % r_in(
       <- plot(x,y),
     %),
     write( 'Press Return to continue...' ), nl,
     read_line_to_codes( user_input, _ ),
     r_devoff, % r_print( 'dev.off()' ),
     Y <- y,
     write( y(Y) ), nl,
     findall( Zx, between(1,9,Zx), Z ),
     z <- Z,
     % r_print( z ),
     cars <- c(1, 3, 6, 4, 9),
     % r_in(
	 <- pie(cars),
     % ),
     write( 'Press Return to continue...' ), nl,
     read_line_to_codes( user_input, _ ),
     r_close.

%%   rbessel
%
%    Demo of plotting Bessel function in R
rbessel :-
    r_open_session,
    Input mapdot sqrt ~> 2.*[0.00001,0.0001,0.001,0.01,0.1,1.0,10.0,100.0],
    y <- besselK(Input,1),
    Y <- y,
    Output mapdot Input * Y,
    write(Output), nl,
    % write( y(Y) ), nl,
    r_close.

%%   delayed_exponent_R(L, Alpha, S, R)
%
%    Demo of delayed exponent in R
delayed_exponent_R(L, Alpha, S, R) :-
    Theta is -L*S,
    num <- complex(modulus=1.0, argument=Theta),
    den <- complex(real=Alpha, imaginary=S),
    r <- num/den,
    R = r.
% retract(num), retract(den).

%%   two_level_model_R(+L1, +Alpha1, +L2, +Alpha2, +S, -R)
%
%    Demo of computing semi-Markov Model in R
two_level_model_R(L1, Alpha1, L2, Alpha2, S, R) :-
    delayed_exponent_R(L1, Alpha1, S, P),
    delayed_exponent_R(L2, Alpha2, S, Q),
    a <- 1.0-P,
    b <- 1.0-Q,
    c <- 1.0-P*Q,
    r <- 'Re'(a*b/c/S^2),
    R <- r.
% retract(a), retract(b), retract(c), retract(r).

frequencies(S, _, Result, Result) :-
    S < 0.01.
frequencies(S, linear, L, Result) :-
    S1 is 0.95*S,
    frequencies(S1, linear, [S|L], Result).
frequencies(S, log, L, Result) :-
    S1 is 0.95*S,
    LogS is log(S1),
    frequencies(S1, log, [LogS|L], Result).

%%   two_level_spectrum_R(+L1, +Alpha1, +L2, +Alpha2, +S, +Init, -Result)
%
%    Demo of computing spectrum in R
two_level_spectrum_R(_, _, _, _, S, Result, Result) :-
    S < 0.01.
two_level_spectrum_R(L1, Alpha1, L2, Alpha2, S, L, Result) :-
    two_level_model_R(L1, Alpha1, L2, Alpha2, S, R),
    S1 is 0.95*S,
    LogR is log(R),
    two_level_spectrum_R(L1, Alpha1, L2, Alpha2, S1, [LogR|L], Result).

%%   dquote(+X, -Y)
%
%    Double quote a string
dquote(X, Y) :-
    atomic_list_concat(['".', X, '"'], Y).

% :- r_open([with(non_interactive)]).

%%   r_complex(+FileName)
%
%    Demo of write a plot to fileusing R
r_complex(FileName) :-
    r_open_session, % ([with(non_interactive)]),
    S is 100.0,
    Alpha1 is 1.0,
    L1 is 1.0,
    Alpha2 is 1.0,
    L2 is 1.0,
    frequencies(S, log, [], F),
    two_level_spectrum_R(L1, Alpha1, L2, Alpha2, S, [], Result),
    x <- F,
    y <- Result,
    !,
    dquote(FileName, FN),
    %r_in(
       <- bmp(filename=FN),
    %),
    % r_in(
       <- plot(x,y),
    % ),
    % r_in(
       r_devoff, % <- 'dev.off()',
    %),
    r_close.

%%   r_app(+Request)
%
%    R demo page
r_app(_) :-
    FN = '/html/images/psd.bmp',
    r_complex(FN),
    reply_html_page(title('Chart'),
                   [ p('PSD from R'),
                     img(src(FN))
                   ]).










