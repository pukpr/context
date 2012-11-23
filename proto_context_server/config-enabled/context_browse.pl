:- module(context_browse, []).

/** <module> Tree browser for context models
    * Uses ontological classification scheme
*/

:- context:register(context_browse:browse).


create_list([]) --> [].
create_list([First|Rest]) -->
    { findall(Next, rdf_(First, child, Next), List) },
    html([ li([a(href('browse?term='+First), First),
	       ul([class(tree)],
		   \(create_list(List))
		  )
	      ]
	     ),
	   \(create_list(Rest))
	 ]
	).

view_tree(Request, Subject) -->
    { findall(Class, rdf_(Subject, child, Class), List),
      rdf_(Subject, comment, Comment),
      rdf_(Subject, link, Link),
      rdf_(Subject, target, Target) % if not found use default
    },
    html_requires(css('ul_tree.css')),
    html(
	[
	table([border=0, width='100%'],
	      [tr([
		   td([width='45%'],
		      [table([border=0, width='100%'],
			  [tr([
			       td([valign(top), width('45%')],
				  [a([href=browse],
				     [
				      \(con_text:gif(browse)),
				      b(Subject)
				     ]
				    ),
				   ul([class(tree)],
				      \(create_list(List))
				     ),
				   p(['< [', a(href('javascript:history.back(1);'), 'back'), ']']) % use con_text version
				  ]),
			       td([valign(top)],
				  [ p(Comment),
				    p(a([href(Link),
					 target(Target)],
					[ img(src('/html/images/rarrow.gif')), 'Go to ', Subject, ' models'])
				     )			])
			      ]),
			   tr([
			       td( [colspan='2'],
				   \(con_text:render_iframe(render)) )
			      ]
			     )
			   ]
			  )]),
		     td( [valign(top)],
			 [
			 \(con_text:render_iframe(Request, 500))
			 ])
		    ])

		])

         ]
        ).

browse(Request) :-
    http_parameters(Request, [term(Term, [string, default(context)])]),
    reply_html_page(
        cliopatria(default),
        [title('Context Browse Models')],
        [
         \(view_tree(Request,Term))
	]
		   ).



