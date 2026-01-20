:- module(context_r, [
		      r_open_session/0,
		      dquote/2,
		      rhist2d/2,
		      rplot/2,
		      rplot_with_regression/7,
		      r_close/0
		     ]).

/** <module> Interfacing with R
    * Bessel function
    * Comparison to other math
*/

:- use_module(context_complex).
:- use_module(context_math).

:- op(400, xfx, ~).

%%   r_open_session
%
%    Open an R session, needed for Linux
r_open_session :-
    % getenv('OS', 'Windows_NT'),
    true. % current_prolog_flag(windows, true),
          % r_open([]), !.
r_open_session :-
    true. % r_open([with(non_interactive)]).


r_close :- true.


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
     % r_in(
	 <- library(gplots),
     %),
     %r_in(
	 <- x11(width=5,height=3.5),
     % ),
     %r_in(
	 <- hist2d(x,y, nbins=20),
     %),
     write( 'Press Return to continue...' ), nl,
     read_line_to_codes( user_input, _ ),
     r_devoff, %    <- 'dev.off()',
     r_close.

%%   rplot(+X,+Y)
%
%    XY Plot using R
rplot(X,Y) :-
     r_open_session,
     y <- Y,
     x <- X,
     % r_in(
       <- x11(width=5,height=3.5),
     % ),
     % r_in(
       <- plot(x,y),
     % ),
     write( 'Press Return to continue...' ), nl,
     read_line_to_codes( user_input, _ ),
     r_devoff, %   <- 'dev.off()',
     r_close.

%%   rplot_with_regression(+Image, +X, +Y, +Title, +X_Axis, +Y_Axis, +Slope)
%
%    Plot with linear regression using pure Prolog (no R dependency)
rplot_with_regression(Image, X, Y, Title, X_Axis, Y_Axis, Slope) :-
     % Calculate linear regression in pure Prolog
     linear_regression(X, Y, Slope, Intercept),
     % Generate SVG plot
     generate_svg_plot(Image, X, Y, Title, X_Axis, Y_Axis, Slope, Intercept).

%%   linear_regression(+X, +Y, -Slope, -Intercept)
%
%    Calculate linear regression coefficients using least squares method
linear_regression(X, Y, Slope, Intercept) :-
     length(X, N),
     length(Y, N),  % Ensure X and Y have same length
     N > 0,
     sum_list(X, SumX),
     sum_list(Y, SumY),
     MeanX is SumX / N,
     MeanY is SumY / N,
     % Calculate slope: sum((x_i - mean_x) * (y_i - mean_y)) / sum((x_i - mean_x)^2)
     calculate_covariance(X, Y, MeanX, MeanY, Cov),
     calculate_variance(X, MeanX, VarX),
     % Handle degenerate case where all X values are identical
     (VarX =:= 0 -> 
          Slope = 0,
          Intercept = MeanY
     ;
          Slope is Cov / VarX,
          Intercept is MeanY - Slope * MeanX
     ).

%%   calculate_covariance(+X, +Y, +MeanX, +MeanY, -Cov)
%
%    Calculate covariance for regression
calculate_covariance(X, Y, MeanX, MeanY, Cov) :-
     calculate_covariance(X, Y, MeanX, MeanY, 0, Cov).

calculate_covariance([], [], _, _, Acc, Acc).
calculate_covariance([Xh|Xt], [Yh|Yt], MeanX, MeanY, Acc, Cov) :-
     Term is (Xh - MeanX) * (Yh - MeanY),
     NewAcc is Acc + Term,
     calculate_covariance(Xt, Yt, MeanX, MeanY, NewAcc, Cov).

%%   calculate_variance(+X, +Mean, -Var)
%
%    Calculate variance
calculate_variance(X, Mean, Var) :-
     calculate_variance(X, Mean, 0, Var).

calculate_variance([], _, Acc, Acc).
calculate_variance([Xh|Xt], Mean, Acc, Var) :-
     Term is (Xh - Mean) * (Xh - Mean),
     NewAcc is Acc + Term,
     calculate_variance(Xt, Mean, NewAcc, Var).

%%   generate_svg_plot(+Image, +X, +Y, +Title, +X_Axis, +Y_Axis, +Slope, +Intercept)
%
%    Generate SVG plot with scatter points and regression line
generate_svg_plot(Image, X, Y, Title, X_Axis, Y_Axis, Slope, Intercept) :-
     % Ensure atoms for display
     (atom(Title) -> TitleAtom = Title ; atom_string(TitleAtom, Title)),
     (atom(X_Axis) -> XAxisAtom = X_Axis ; atom_string(XAxisAtom, X_Axis)),
     (atom(Y_Axis) -> YAxisAtom = Y_Axis ; atom_string(YAxisAtom, Y_Axis)),
     
     % Calculate plot dimensions and ranges
     min_list(X, MinX),
     max_list(X, MaxX),
     min_list(Y, MinY),
     max_list(Y, MaxY),
     
     % Add padding to ranges
     XRange is MaxX - MinX,
     YRange is MaxY - MinY,
     PlotMinX is MinX - XRange * 0.1,
     PlotMaxX is MaxX + XRange * 0.1,
     PlotMinY is MinY - YRange * 0.1,
     PlotMaxY is MaxY + YRange * 0.1,
     
     % SVG dimensions (constants for standard plot size)
     svg_plot_dimensions(Width, Height, MarginLeft, MarginRight, MarginTop, MarginBottom),
     PlotWidth is Width - MarginLeft - MarginRight,
     PlotHeight is Height - MarginTop - MarginBottom,
     
     % Ensure parent directory exists before opening file
     file_directory_name(Image, Dir),
     (exists_directory(Dir) -> true ; make_directory_path(Dir)),
     
     % Open file for writing
     open(Image, write, Stream),
     
     % Write SVG header
     format(Stream, '<?xml version="1.0" encoding="UTF-8"?>~n', []),
     format(Stream, '<svg xmlns="http://www.w3.org/2000/svg" width="~w" height="~w">~n', [Width, Height]),
     format(Stream, '<rect width="~w" height="~w" fill="white"/>~n', [Width, Height]),
     
     % Write title
     TitleX is Width / 2,
     format(Stream, '<text x="~w" y="30" text-anchor="middle" font-size="18" font-weight="bold">~w</text>~n', 
            [TitleX, TitleAtom]),
     
     % Write axes labels
     XLabelX is MarginLeft + PlotWidth / 2,
     XLabelY is Height - 20,
     format(Stream, '<text x="~w" y="~w" text-anchor="middle" font-size="14">~w</text>~n', 
            [XLabelX, XLabelY, XAxisAtom]),
     
     YLabelX = 20,
     YLabelY is MarginTop + PlotHeight / 2,
     format(Stream, '<text x="~w" y="~w" text-anchor="middle" font-size="14" transform="rotate(-90 ~w ~w)">~w</text>~n', 
            [YLabelX, YLabelY, YLabelX, YLabelY, YAxisAtom]),
     
     % Draw plot area border
     format(Stream, '<rect x="~w" y="~w" width="~w" height="~w" fill="none" stroke="black" stroke-width="2"/>~n',
            [MarginLeft, MarginTop, PlotWidth, PlotHeight]),
     
     % Draw grid lines - Y axis every 10 days, X axis every 20 years
     draw_grid_lines(Stream, PlotMinX, PlotMaxX, PlotMinY, PlotMaxY,
                    MarginLeft, MarginTop, PlotWidth, PlotHeight),
     
     % Draw regression line
     LineY1 is Intercept + Slope * PlotMinX,
     LineY2 is Intercept + Slope * PlotMaxX,
     scale_coordinates(PlotMinX, LineY1, PlotMinX, PlotMaxX, PlotMinY, PlotMaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight, LineX1, LineY1Scaled),
     scale_coordinates(PlotMaxX, LineY2, PlotMinX, PlotMaxX, PlotMinY, PlotMaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight, LineX2, LineY2Scaled),
     format(Stream, '<line x1="~w" y1="~w" x2="~w" y2="~w" stroke="blue" stroke-width="2"/>~n',
            [LineX1, LineY1Scaled, LineX2, LineY2Scaled]),
     
     % Draw data points
     draw_points(Stream, X, Y, PlotMinX, PlotMaxX, PlotMinY, PlotMaxY,
                MarginLeft, MarginTop, PlotWidth, PlotHeight),
     
     % Close SVG
     format(Stream, '</svg>~n', []),
     close(Stream).

%%   svg_plot_dimensions(-Width, -Height, -MarginLeft, -MarginRight, -MarginTop, -MarginBottom)
%
%    Standard SVG plot dimensions
svg_plot_dimensions(800, 600, 80, 40, 60, 80).

%%   scale_coordinates(+DataX, +DataY, +MinX, +MaxX, +MinY, +MaxY, 
%%                     +MarginLeft, +MarginTop, +PlotWidth, +PlotHeight, -SvgX, -SvgY)
%
%    Convert data coordinates to SVG coordinates
scale_coordinates(DataX, DataY, MinX, MaxX, MinY, MaxY,
                 MarginLeft, MarginTop, PlotWidth, PlotHeight, SvgX, SvgY) :-
     XRange is MaxX - MinX,
     YRange is MaxY - MinY,
     % When range is 0 (all values identical), center the point at 0.5
     (XRange =:= 0 -> NormX = 0.5 ; NormX is (DataX - MinX) / XRange),
     (YRange =:= 0 -> NormY = 0.5 ; NormY is (DataY - MinY) / YRange),
     SvgX is MarginLeft + NormX * PlotWidth,
     % SVG Y coordinates are inverted (0 is at top)
     SvgY is MarginTop + (1 - NormY) * PlotHeight.

%%   draw_points(+Stream, +X, +Y, +MinX, +MaxX, +MinY, +MaxY,
%%               +MarginLeft, +MarginTop, +PlotWidth, +PlotHeight)
%
%    Draw scatter plot points
draw_points(_, [], [], _, _, _, _, _, _, _, _).
draw_points(Stream, [Xh|Xt], [Yh|Yt], MinX, MaxX, MinY, MaxY,
           MarginLeft, MarginTop, PlotWidth, PlotHeight) :-
     scale_coordinates(Xh, Yh, MinX, MaxX, MinY, MaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight, SvgX, SvgY),
     format(Stream, '<circle cx="~w" cy="~w" r="4" fill="red"/>~n', [SvgX, SvgY]),
     draw_points(Stream, Xt, Yt, MinX, MaxX, MinY, MaxY,
                MarginLeft, MarginTop, PlotWidth, PlotHeight).

%%   draw_grid_lines(+Stream, +MinX, +MaxX, +MinY, +MaxY,
%%                   +MarginLeft, +MarginTop, +PlotWidth, +PlotHeight)
%
%    Draw grid lines - Y axis every 10 days, X axis every 20 years
draw_grid_lines(Stream, MinX, MaxX, MinY, MaxY,
               MarginLeft, MarginTop, PlotWidth, PlotHeight) :-
     % Draw Y-axis grid lines every 10 days
     StartY is ceiling(MinY / 10) * 10,
     EndY is floor(MaxY / 10) * 10,
     draw_y_grid_lines(Stream, StartY, EndY, MinX, MaxX, MinY, MaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight),
     
     % Draw X-axis grid lines every 20 years
     StartX is ceiling(MinX / 20) * 20,
     EndX is floor(MaxX / 20) * 20,
     draw_x_grid_lines(Stream, StartX, EndX, MinX, MaxX, MinY, MaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight).

%%   draw_y_grid_lines(+Stream, +CurrentY, +EndY, +MinX, +MaxX, +MinY, +MaxY,
%%                     +MarginLeft, +MarginTop, +PlotWidth, +PlotHeight)
%
%    Draw horizontal grid lines for Y axis
draw_y_grid_lines(Stream, CurrentY, EndY, MinX, MaxX, MinY, MaxY,
                 MarginLeft, MarginTop, PlotWidth, PlotHeight) :-
     CurrentY =< EndY,
     !,
     % Draw grid line
     scale_coordinates(MinX, CurrentY, MinX, MaxX, MinY, MaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight, X1, Y),
     scale_coordinates(MaxX, CurrentY, MinX, MaxX, MinY, MaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight, X2, _),
     format(Stream, '<line x1="~w" y1="~w" x2="~w" y2="~w" stroke="lightgray" stroke-width="1" stroke-dasharray="3,3"/>~n',
            [X1, Y, X2, Y]),
     
     % Draw label
     LabelX is MarginLeft - 10,
     format(Stream, '<text x="~w" y="~w" text-anchor="end" font-size="10" dy="0.3em">~w</text>~n',
            [LabelX, Y, CurrentY]),
     
     % Continue with next grid line
     NextY is CurrentY + 10,
     draw_y_grid_lines(Stream, NextY, EndY, MinX, MaxX, MinY, MaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight).
draw_y_grid_lines(_, _, _, _, _, _, _, _, _, _, _).

%%   draw_x_grid_lines(+Stream, +CurrentX, +EndX, +MinX, +MaxX, +MinY, +MaxY,
%%                     +MarginLeft, +MarginTop, +PlotWidth, +PlotHeight)
%
%    Draw vertical grid lines for X axis
draw_x_grid_lines(Stream, CurrentX, EndX, MinX, MaxX, MinY, MaxY,
                 MarginLeft, MarginTop, PlotWidth, PlotHeight) :-
     CurrentX =< EndX,
     !,
     % Draw grid line
     scale_coordinates(CurrentX, MinY, MinX, MaxX, MinY, MaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight, X, Y1),
     scale_coordinates(CurrentX, MaxY, MinX, MaxX, MinY, MaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight, _, Y2),
     format(Stream, '<line x1="~w" y1="~w" x2="~w" y2="~w" stroke="lightgray" stroke-width="1" stroke-dasharray="3,3"/>~n',
            [X, Y1, X, Y2]),
     
     % Draw label
     LabelY is MarginTop + PlotHeight + 20,
     format(Stream, '<text x="~w" y="~w" text-anchor="middle" font-size="10">~w</text>~n',
            [X, LabelY, CurrentX]),
     
     % Continue with next grid line
     NextX is CurrentX + 20,
     draw_x_grid_lines(Stream, NextX, EndX, MinX, MaxX, MinY, MaxY,
                      MarginLeft, MarginTop, PlotWidth, PlotHeight).
draw_x_grid_lines(_, _, _, _, _, _, _, _, _, _, _).

pipe_interrupt(_Sig) :-
     r_close.

:- on_signal(13, _Current, context_r:pipe_interrupt).

