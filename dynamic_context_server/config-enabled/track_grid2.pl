:- module(track_grid2, []).
:- context:register(track_grid2:navigate).
:- context:register(track_grid2:plot).

%  These are the available types of tiles
tile(blank).
tile(cross).
tile(curveNE).
tile(curveNW).
tile(curveSE).
tile(curveSW).
tile(lineE).
tile(lineS).
tile(parkE).
tile(parkN).
tile(parkS).
tile(parkW).
tile(teeE).
tile(teeN).
tile(teeS).
tile(teeW).

substitute_group([curveNE,curveNW,curveSE,curveSW]).
substitute_group([parkE,parkN,parkS,parkW]).
substitute_group([teeE,teeN,teeS,teeW]).
substitute_group([lineE,lineS]).
substitute_group([blank]).
substitute_group([cross]).


% This is the database of tiles = a(Front,Back)
a(curveSE, lineE).
a(teeS, lineE).
a(curveSW, curveNE). % NE
a(curveSW, parkN).
a(blank, lineS).
a(curveSE, curveNW).
a(curveSE, teeS).
a(teeW, curveNE).
a(curveSW, teeE).
a(cross, curveNW).
a(parkE, teeN).
a(curveNW, parkN).
a(blank, cross).


a(curveSE, lineE).
a(teeS, lineE).
a(curveSW, curveNE).
a(curveSW, parkN).
a(blank, lineS).
a(curveSE, curveNW).
a(curveSE, teeS).
a(teeW, curveNE).
a(curveSW, teeE).
a(cross, curveNW).
a(parkE, teeN).
a(curveNW, parkN).
a(blank, blank).


/*
a(curveSE, lineE).
a(teeS, lineE).
a(teeW, curveNE).
a(curveNW, parkN).
a(curveSE, teeS).
a(cross, curveNW).
a(curveSW, teeE).
a(parkE, teeN).
a(curveSW, curveNE).
a(curveSW, parkN).
a(blank, lineS).
a(curveSE, curveNW).
a(blank, cross).

a(curveSE, lineE).
a(teeS, lineE).
a(curveSW, curveNE).
a(curveSW, parkN).
a(blank, lineS).
a(curveSE, curveNW).
a(curveSE, teeS).
a(teeW, curveNE).
a(curveSW, teeE).
a(cross, curveNW).
a(parkE, teeN).
a(curveNW, parkN).
a(blank, blank).

a(curveSE, lineE).
a(teeS, lineE).
a(curveSW, curveNE).
a(curveSW, parkN).
a(blank, lineS).
a(curveSE, curveNW).
a(curveSE, teeS).
a(teeW, curveNE).
a(curveSW, teeE).
a(cross, curveNW).
a(parkE, teeN).
a(curveNW, parkN).
a(blank, blank).

*/


sub_group(Piece, Sub) :-
   substitute_group(G),
   member(Piece, G),
   member(Sub, G).

either_side(Front, [Sub,_]) :- sub_group(Front, Sub).
either_side(Back,  [_,Sub]) :- sub_group(Back,  Sub).

config(0,
       [curveSE, curveSW,
	curveNE, curveNW]).

config(100,
       [cross, cross,
	cross, cross]).

config(1,
       [curveSE, lineE, teeS, lineE, curveSW,
	curveNE, curveSW, parkN, blank, lineS,
	curveSE, curveNW, curveSE, teeS, teeW,
	curveNE, curveSW, teeE, cross, curveNW,
	parkE, teeN, curveNW, parkN, blank]).

config(2,
       [parkE, lineE, teeS, curveSW, blank,
	curveSE, teeS, cross, curveNW, lineS,
	lineS, lineS , lineS, blank, lineS,
	curveNE, teeN, cross, lineE, teeW,
	blank, parkE, curveNW, blank, parkN]).


available([], _, 'all parts available!') :- !.
available([N|Needed], From, Result) :-
   % member(Piece, From),
   either_side(N, Piece),
   member(Piece, From),
   selectchk(Piece, From, Remainder),
   available(Needed, Remainder, Result).
% available([N|_], _, Text) :-
%   concat(N, ' extra parts needed!', Text), !.

find_available(Plan, From,  Result) :-
   findall([Front,Back], a(Front,Back), From ), !,
   available(Plan, From,  Result), !.

make_gif(Part, GIF) :-
   atomic_list_concat(['/html/images/', Part, '.gif'], GIF).

img_tile(GIF) :-
    tile(Part),
    make_gif(Part, GIF).

navigate(Request) :-
   findall(img([width='50',src(A),border=2,vspace=2, hspace=2]), img_tile(A), Parts),
   findall([Front,Back], a(Front,Back), List ),
   reply_html_page(cliopatria(default),
		   [title('Tracks')],
		   [\(con_text:table_with_iframe_target(
				   Request,
				   [
				    h1('Track Solver'),
				    p('Pick configuration: 1,2,.. etc'),
				    form([action(plot), target(target_iframe)],
					 [
					  input([type('text'), name('x'), value('1')]),
					  input([type('submit'), name(solve), value('Buildable?')])
					 ]
					),
				    br([]),
				    h3('Defined Tile Types'),
				    b(Parts),
				    h3('Available Tiles as Front + Back pairings'),
				    \(track_grid2:show_each_part(List)),
				    \(con_text:render_iframe(render))
				   ]
						       ))
		   ]
		  ).

%
% Charting routines
%
process_each(N, L, Part) -->
   {
    make_gif(Part, GIF),
    M is N - 1,
    XPos is 100*(M mod L),
    YPos is 100*(M div L),
    format(atom(Style),'position:absolute; top:~wpx; left:~wpx; width:100px; height:100px; border:none;', [YPos,XPos])
   },
   html(
       [img([src=GIF,
	     style=Style])
       ]).

% process(0,'none') --> html([p(none)]), !.
process(_,_,[]) --> !.
process(N,L,[First|Rest]) -->
   {M is N +1},
   html([
          div([style('position:relative;')],
	   [
	    \process_each(M, L, First)
	   ]
	  )
	]),
   process(M, L, Rest).


show_each_part([]) --> !.
show_each_part([[F,B]|Rest]) -->
   {
	make_gif(F, FGIF),
	make_gif(B, BGIF)
   },
   html(
       [img([src=FGIF, width=50]), b(' flip-over '),
	img([src=BGIF, width=50]),
	p([])
       ]),
   show_each_part(Rest).


plot(Request) :-
    http_parameters(Request, [x(X, [number])]),
    config(X,Plan),
    length(Plan, Length),
    Len is integer(sqrt(Length)),
    find_available(Plan,_List, Result),!,
    reply_html_page([title('Track Layout'),
                     \(con_text:style)],
                    [
		     h1([Result]),
		     \process(0, Len, Plan)
		     % \show_each_part(Plan)
                    ]
		  ).

