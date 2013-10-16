:- module(context_r, [
		      r_open_session/0,
		      dquote/2,
		      rhist2d/2,
		      rplot/2,
		      rplot_with_regression/7
		     ]).

/** <module> Interfacing with R
    * Bessel function
    * Comparison to other math
*/

:- use_module(context_complex).
:- use_module(library('R')).
:- use_module(context_math).

:- op(400, xfx, ~).

%%   r_open_session
%
%    Open an R session, needed for Linux
r_open_session :-
    % getenv('OS', 'Windows_NT'),
    current_prolog_flag(windows, true),
    r_open([]), !.
r_open_session :-
    r_open([with(non_interactive)]).

%%   dquote(+X, -Y)
%
%    Double-quote () a term
dquote(X, Y) :-
    atomic_list_concat(['".', X, '"'], Y).

%%   rhist2d(+X,+Y)
%
%    2D histogram Plot using R
rhist2d(X,Y) :-
     r_open_session,
     y <- Y,
     x <- X,
     r_in( library(gplots) ),
     r_in( x11(width=5,height=3.5) ),
     r_in( hist2d(x,y, nbins=20) ),
     write( 'Press Return to continue...' ), nl,
     read_line_to_codes( user_input, _ ),
     r_print( 'dev.off()' ),
     r_close.

%%   rplot(+X,+Y)
%
%    XY Plot using R
rplot(X,Y) :-
     r_open_session,
     y <- Y,
     x <- X,
     r_in( x11(width=5,height=3.5) ),
     r_in( plot(x,y) ),
     write( 'Press Return to continue...' ), nl,
     read_line_to_codes( user_input, _ ),
     r_print( 'dev.off()' ),
     r_close.

%%   rplot_with_regression(+Image, +X, +Y, +Title, +X_Axis, +Y_Axis, +Slope)
%
%    Demo rplot with regression
rplot_with_regression(Image, X, Y, Title, X_Axis, Y_Axis, Slope) :-
     r_open_session,
     y <- Y,
     x <- X,
     fitxy <- lm('y~x'),
     r_print(fitxy),
     Slope <- 'as.double(fitxy$coefficients[2])',
     dquote(Image, FN),
     r_in( bmp(filename=FN)),
     % r_in( x11(width=5,height=3.5) ),
     r_in( plot(x,y,xlab=X_Axis,ylab=Y_Axis,main=Title) ),
     % r_in( summary(fitxy) ),
     r_in( abline(fitxy) ),
     r_print( 'dev.off()' ),
     r_close.

pipe_interrupt(_Sig) :-
     r_close.

:- on_signal(13, _Current, context_r:pipe_interrupt).

