:- module(track_grid, []).
:- context:register(track_grid:navigate).
:- context:register(track_grid:plot).


/*
show_each_part([]) --> !.
show_each_part([p(Part)|Rest]) -->
   {
	make_gif(Part, GIF)
   },
   html(
       [img(
	    [src=GIF]
	   )
       ]),
   show_each_part(Rest).
*/

img_part(GIF) :-
    p(Part),
    make_gif(Part, GIF).

navigate(Request) :-
   findall(img([width='50',src(A),border=2,vspace=2, hspace=2]), img_part(A), Parts),
   reply_html_page(cliopatria(default),
		   [title('Tracks')],
		   [\(con_text:table_with_iframe_target(
				   Request,
				   [
				    h1('Track Solver'),
				    p('Enter over and down tiles'),
				    form([action(plot), target(target_iframe)],
					 [
					  input([type('text'), name('x'), value('2')]), i(' <= x'),  br([]),
					  input([type('text'), name('y'), value('4')]), i(' <= y'),  br([]),
					  input([type('submit'), name(solve), value('Solve')])
					 ]
					),
				    br([]),
				    h3('Available Tiles'),
				    b(Parts),
				    \(con_text:render_iframe(render))
				   ]
						       ))
		   ]
		  ).


/* Parts */
p([0,1,1,0]).
p([0,0,1,1]).
p([1,1,0,0]).
p([0,1,0,1]).
p([0,0,1,1]).
p([1,0,1,0]).
p([1,0,0,1]).
p([0,1,0,1]).
p([0,1,0,1]).
p([1,1,0,0]).
p([1,0,1,0]).
p([1,0,1,0]).



/* Combination Rule */
right([_,1,_,_],[_,0,_,1]).
below([_,_,1,_],[1,_,_,0]).

/* Grow Combinations by Linking */
% link(X, X, Y, Y, _,_,Final,R) :- reverse(Final, R), !.
link(X, X, Y, Y, _,_,Final,Final) :- !.
link(X, XMax, Y, YMax, M1, Parts, NewList, Final) :-
   member(M2,Parts),
   right(M1,M2),
   select(M2,Parts,ReducedParts),
   XM is X + 1,
   link(XM, XMax, Y, YMax, M2, ReducedParts, [part(XM,Y,M2)|NewList], Final).
link(X, XMax, Y, YMax, M1, Parts, NewList, Final) :-
   member(M2,Parts),
   below(M1,M2),
   select(M2,Parts,ReducedParts),
   YM is Y + 1,
   link(X, XMax, YM, YMax, M2, ReducedParts, [part(X,YM,M2)|NewList], Final).


/* Solve top-level */
run(X,Y,Final) :-
   findall(A,p(A),L),
   member(M1,L),
   link(0, X, 0, Y, M1, L, [part(0,0,M1)], Final).

make_gif(Part, GIF) :-
   atomic_list_concat(Part,Name),
   atomic_list_concat(['/html/images/', Name, '.gif'], GIF).

%
process_each([]) --> !.
process_each([part(X, Y, Part)|Rest]) -->
   {
    make_gif(Part, GIF),
    XPos is 100+X*24,
    YPos is Y*24,
    format(atom(Style),'position:absolute; top:~wpx; left:~wpx; width:24px; height:24px; border:none;', [YPos,XPos])
   },
   html(
       [img([src=GIF,
	     style=Style])
       ]),
   process_each(Rest).

process(0,'none') --> html([p(none)]), !.
process(_,[]) --> !.
process(N,[First|Rest]) -->
   {M is N +1},
   html([
          div([style('position:relative;')],
	   [
	    \process_each(First)
	   ]
	  ),
       h1(M), h1('.'), h1('.')
	]),
   process(M, Rest).


plot(Request) :-
    http_parameters(Request, [x(X, [number]),
                              y(Y, [number])]),
    (	setof(L,run(X, Y, L),S) ;
         S='none'),
    reply_html_page([title('Track Layout'),
                     \(con_text:style)],
                    [
		     h1(['over=', X, ' down=', Y]),
		     \process(0, S)
                    ]
		  ).

