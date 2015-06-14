:- module(track_grid2, []).
:- context:register(track_grid2:navigate).
:- context:register(track_grid2:plot).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  These are the available types of tiles
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
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


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the database of tiles = a(Front,Back)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
a(curveSE, lineE).
a(teeS, lineE).
a(lineS, curveNE).
a(curveSW, parkN).
a(blank, lineS).
a(lineE, curveNW).
a(curveSE, teeS).
a(teeW, curveNE).
a(curveSW, teeE).
a(cross, curveNW).
a(parkE, teeN).
a(curveNW, parkN).
a(blank, cross).


a(curveSE, lineE).
a(teeS, lineE).
a(curveSW, blank).
a(curveSW, parkN).
a(blank, lineS).
a(lineE, curveNW).
a(curveSE, teeS).
a(teeW, curveNE).
a(curveSW, teeE).
a(cross, curveNW).
a(parkE, teeN).
a(curveNW, parkN).
a(blank, blank).

a(curveSE, lineE).
a(teeS, lineE).
a(teeW, curveNE).
a(curveNW, parkN).
a(curveSE, teeS).
a(cross, curveNW).
a(curveSW, teeE).
a(parkE, teeN).
a(blank, curveNE).
a(curveSW, parkN).
a(blank, lineS).
a(curveSE, curveNW).
a(blank, cross).

a(curveSE, lineE).
a(teeS, lineE).
a(cross, curveNE).
a(curveSW, parkN).
a(blank, lineS).
a(blank, curveNW).
a(curveSE, teeS).
a(teeW, curveNE).
a(curveSW, teeE).
a(cross, curveNW).
a(parkE, teeN).
a(curveNW, parkN).
a(blank, blank).

a(curveSE, lineE).
a(teeS, lineE).
a(blank, curveNE).
a(curveSW, parkN).
a(blank, lineS).
a(curveSE, curveNW).
a(curveSE, teeS).
a(teeW, curveNE).
a(curveSW, teeE).
a(cross, curveNW).
a(parkE, teeN).
a(curveNW, parkN).

a(blank, lineS).
a(blank, lineE).
a(lineS, blank).
a(lineS, blank).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are track configurations
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

config(0,
       [curveSE, curveSW,
	curveNE, curveNW]).

config(100,
       [cross, cross,
	cross, cross]).
config(1000,
       [curveSE, lineE, curveSW,
	lineS, blank, lineS,
        curveNE, lineE, curveNW ]).

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

config(3,
       [curveSE, lineE, teeS, lineE, curveSW,
	lineS, blank, lineS, blank, lineS,
	teeE, lineE , cross, lineE, teeW,
	lineS, blank, lineS, blank, lineS,
	curveNE, lineE, teeN, lineE, curveNW]).

config(4,
       [parkS, curveSE, teeS, lineE, parkW,
	teeE, curveNW, curveNE, teeS, curveSW,
	lineS, blank , blank, teeE, teeW,
	teeE, curveSW, curveSE, teeN, curveNW,
	parkN, curveNE, teeN, lineE, parkW]).

config(5,
       [curveSE, lineE, teeS, lineE, curveSW,
	teeE, parkW, lineS, parkS, lineS,
	lineS, blank, lineS, lineS, lineS,
	teeE, parkW, curveNE, cross, teeW,
	curveNE, lineE, lineE, teeN, curveNW]).

config(6,
       [curveSE, curveSW, parkS, curveSE, curveSW,
	curveNE, cross, teeN, cross, curveNW,
	blank, lineS, blank, lineS, blank,
	curveSE, cross, teeS, cross, curveSW,
	curveNE, curveNW, parkN, curveNE, curveNW]).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are the rules
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sub_group(Piece, Sub) :-
   substitute_group(G),
   member(Piece, G),
   member(Sub, G).

either_side(Front, [Sub,_]) :- sub_group(Front, Sub).
either_side(Back,  [_,Sub]) :- sub_group(Back,  Sub).


% available([], _, 'All parts available!') :- !.
available([], _, R,RR) :- reverse(R,RR),!.
available([N|Needed], From, List, Result) :-
   % member(Piece, From),
   either_side(N, Piece),
   member(Piece, From),
   select(Piece, From, Remainder),
   available(Needed, Remainder, [Piece|List], Result).
% available([N|_], _, Text) :-
%   concat(N, ' extra parts needed!', Text), !.

find_available(Plan, From,  Result) :-
   findall([Front,Back], a(Front,Back), From ), !,
   available(Plan, From, [], Result).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  User Interface
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    XPos is 100*(M mod L) + 250,
    YPos is 100*(M div L),
    format(atom(Style),
	   'position:absolute; top:~wpx; left:~wpx; width:100px; height:100px; border:none;',
	   [YPos,XPos])
   },
   html(
       [img([src=GIF,
	     style=Style])
       ]).

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
        length(Rest,N),
	make_gif(F, FGIF),
	make_gif(B, BGIF)
   },
   html(
       [i(N), img([src=FGIF, width=50]), b(' flip-over '),
	img([src=BGIF, width=50]),
	p([])
       ]),
   show_each_part(Rest).

show_each_part([], _, _) --> !.
show_each_part([[F,B]|Rest], From, Members) -->
   {
        % length(Rest,N),
	make_gif(F, FGIF),
	make_gif(B, BGIF),
        nth0(N,From,[F,B]),
        not(member(N,Members))
   },
   html(
       [i(N), img([src=FGIF, width=50]), b(' flip-over '),
	img([src=BGIF, width=50]),
	p([])
       ]),
   show_each_part(Rest,From, [N|Members]).

plot(Request) :-
    http_parameters(Request, [x(X, [number])]),
    config(X,Plan),
    length(Plan, Length),
    Len is integer(sqrt(Length)),
    find_available(Plan,List, Result),!,
    reverse(List,RList),
    reply_html_page([title('Track Layout'),
                     \(con_text:style)],
                    [
		     h1('Parts used from those available (scroll down)'),
		     \process(0, Len, Plan),
		     \show_each_part(Result,RList,[])
                    ]
		  ).





