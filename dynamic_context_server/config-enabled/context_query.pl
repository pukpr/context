:- module(context_query, [
                          owl/3
                         ]).

:- use_module(context_math).
:- use_module(applications(browse)).

% :- use_module(autocompletion).

% :-init_autocomplete.

:- context:register(context_query:navigate).
:- context:register(context_query:pquery).
:- context:register(context_query:owl_result).


model_elements(option([value(Short)],[Name])) :-
    rdf(ID, rdfs:'subClassOf', _),
    rdf_global_id(ent:Name, ID),
    context:concise_term(ID, Short).

owl(Query, Resource, Title) :-
    rdf(Resource, rdf:type, owl:'Class'),
    upcase_atom(Resource, R),
    upcase_atom(Query, Q),
    sub_atom(R, _,_,_, Q),
    context:concise_term(Resource, Title).
    % Title=Resource.

owl_result(Request) :-
   http_parameters(Request, [result(Result, [])]),
   context:create_global_term(Result, Graph),
   context:uri_index_link(Graph, 'definition', render, GraphLink),
   % context:concise_term(Result, R),
   reply_html_page([title('Def'),
                    \(con_text:style)
                   ],
                   [
                    p([Result, ' : ', GraphLink]),
                    br([]),
                    \(context_graph(Graph, []))
		   ]
		  ).

triple(S,P,O) :-
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
    rdf(S1,P1,O1),
    !.


pquery(Request) :-
    http_parameters(Request, [input(Input, [])]),
    atom_to_term(Input, Terms, Var),
    setof(Var, call(Terms),  Vars),
    % with_output_to(atom(Text), write(Var)),
    reply_html_page(
        [title('Prolog query'),
	 \(con_text:style)],
        [% p(\(con_text:flist(Var))),
	 % p(Text),
	\(con_text:paragraphs(Vars))]
                   ).


navigate(Request) :-
    findall(Ent, model_elements(Ent), Ents),
    sort(Ents, Elements),
    reply_html_page(cliopatria(default),
                   [title('General queries')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('General Queries'),
		      ul([
                          li(p([\(con_text:gif(query)),
                                'Query for model element definitions',
                                form([action('owl_result')
                                     , target=target_iframe
                                     ],
                                     [
                                      select([name('result')], Elements),
                                      input([type('submit')])]
                                    )])
                            ),

                          li(p([\(con_text:gif(query)),
                                'Query OWL Classes',
                                \(con_text:form_ac(owl_result, target_iframe, owl, result))
                               ])
                            ),

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
			     li(a([href('/context_ont_utils/find_ent_objects'),
				   target(target_iframe)], 'Objects'))
			    ])
			       ])
			    ),
			  li(p([\(con_text:gif(query)),'Query via Prolog',
				\(con_text:form('/context_query/pquery',
						target_iframe,
						[[input,'triple(Subject, dc:title, Object)',45]]
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


