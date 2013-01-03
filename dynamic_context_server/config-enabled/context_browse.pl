:- module(context_browse, []).

/** <module> Tree browser for context models
    * Uses ontological classification scheme
*/

:- context:register(context_browse:navigate).


%%   create_list(+L)//
%
%    Part of the browser tree
create_list([]) --> [].
create_list([First|Rest]) -->
    { findall(Next, rdf_(First, child, Next), List) },
    html([ li([a(href('navigate?term='+First), First),
	       ul([class(tree)],
		   \(create_list(List))
		  )
	      ]
	     ),
	   \(create_list(Rest))
	 ]
	).

%%   foundational(+Subject, -Link)
%
%    Link to a foundational article
foundational(Subject, Link) :-
      rdf_(Subject, foundation, [Doc,Page]),
      rdf_(Doc, foundation_doc, FileName),
      context:page_link_to_pdf(FileName, Page, Link), !.
foundational(_Subject, '').


%%   link_view(+Link, +Target, +Image, +Subject, +Text)//
%
%    Link inline view
link_view('', _, _, _Subject, _Docs) -->
    % html(p([b(Subject), ' has no categorized ', Docs])),
    !.

link_view(Link, Target, Image, Subject, Text) -->
    {
     concat('/html/images/', Image, Img)
    },
    html(p(a([href(Link),
              target(Target)],
             [ img([src(Img), width(20)]),
               'Go to ', Subject, Text
             ]
            )
          )),
    !.

%%   view_tree(+Request, +Subject)//
%
%    View browser tree inline
view_tree(Request, Subject) -->
    { findall(Class, rdf_(Subject, child, Class), List),
      rdf_(Subject, comment, Comment),
      rdf_(Subject, link, Link),
      rdf_(Subject, target, Target), % if not found use default
      foundational(Subject, FoundationLink)
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
				  [a([href=navigate],
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
                                    /*
				    p(a([href(Link),
					 target(Target)],
                                        [ img(src('/html/images/rarrow.gif')),
                                          'Go to ', Subject, ' models'
                                        ]
                                       )
                                     ),
				    p(a([href(FoundationLink),
					 target(Target)],
                                        [ img([src('/html/images/acro.gif'),
                                               width(20)]),
                                          'Go to ', Subject, ' foundational document'
                                        ]
                                       )
                                     ),
                                    */
                                    \(link_view(Link, Target, 'rarrow.gif', Subject, ' models')),
                                    \(link_view(FoundationLink, target_iframe, 'acro.gif', Subject, ' foundational document'))
                                  ])
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

%%   navigate(+Request) 
%
%    Dynamic page to feature browser
navigate(Request) :-
    http_parameters(Request, [term(Term, [string, default(context)])]),
    reply_html_page(
        cliopatria(default),
        [title('Context Browse Models')],
        [
         \(view_tree(Request,Term))
	]
		   ).



