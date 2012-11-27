:- module(context_query, [
			    ]).

:- use_module(context_math).
% :- use_module(autocompletion).

% :-init_autocomplete.

:- context:register(context_query:navigate).
:- context:register(context_query:pquery).

/*
cliopatria:menu_item(query/'/context_ont_utils/find_ent_subjects', 'Subjects').
cliopatria:menu_item(query/'/context_ont_utils/find_ent_predicates', 'Predicates').
cliopatria:menu_item(query/'/context_ont_utils/find_ent_objects', 'Objects').
cliopatria:menu_item(query/'/context_file_reading/crawl', 'Models').
cliopatria:menu_item(query/'/terms#', 'Terminology').
*/

t(S,P,O) :-
    (	callable(P) ->
        rdf_global_term(P, P1)
    ;
        P1 = P
    ),
    (	callable(S) ->
        rdf_global_term(S, S1)
    ;
        S1 = S
    ),
    (	callable(O) ->
        rdf_global_term(O, O1)
    ;
        O1 = O
    ),
    rdf(S1,P1,O1).


pquery(Request) :-
    http_parameters(Request, [input(Input, [])]),
    atom_to_term(Input, Terms, Var),
    call(Terms),
    % with_output_to(atom(Text), write(Var)),
    reply_html_page(
        [title('Prolog query'),
	 \(con_text:style)],
        [% p(\(con_text:flist(Var))),
	 % p(Text),
	\(con_text:paragraphs(Var))]
                   ).


navigate(Request) :-
   reply_html_page(cliopatria(default),
                   [title('General queries')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('General Queries'),
		      ul([
			  li(p([\(con_text:gif(query)),
			   a([href('/sparql/?query=select * where{?s relaSci:hasNumericValue ?o}')],
                          'SPARQL query syntax')])),
			  li(p([\(con_text:gif(query)),
			   a([href('/terms#'),
			   target(target_iframe)], 'Terminology')])),
			  li(p([\(con_text:gif(query)),
			   a([href('/context_file_reading/crawl'),
			   target(target_iframe)], 'Other models')])),
			  li(p([\(con_text:gif(query)),
			   'Query all triples:',
			   ol([
			     li(a([href('/context_ont_utils/find_ent_subjects'),
				   target(target_iframe)], 'Subjects')),
			     li(a([href('/context_ont_utils/find_ent_predicates'),
				   target(target_iframe)], 'Predicates')),
			     li(a([href('/context_ont_utils/find_ent_subjects'),
				   target(target_iframe)], 'Objects'))
			    ])
			       ])
			    ),
			  li(p([\(con_text:gif(query)),'Query via Prolog',
				\(con_text:form('/context_query/pquery',
						target_iframe,
						[[input,'t(Subject,dc:title,Object)',35]]
					       ))
			       ]
			      )
			    ),
			  br([]),
			  \(con_text:render_iframe(render))
			 ]
			)
		     ]
						       )
		     )
		   ]
		  ).


