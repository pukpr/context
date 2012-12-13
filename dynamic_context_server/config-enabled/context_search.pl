:- module(context_search, [
                          ]).

/** <module> Context search main
    * General, free-form search
    * Semantic
*/

:- use_module(components(simple_search)).
:- use_module(components(messages)).
:- use_module(applications(browse)).

:- context:register(context_search:navigate).
:- context:register(context_search:my_search_filter).
:- context:register(context_search:list_pages).
:- context:register(context_search:list_cats).
% :- context:register(context_search:owl_result).

find_requirement_topics(option([value(Name)],[Local])) :-
   rdf(Name, rdf:type, req:'Requirement'),
   rdf_split_url(_, Local, Name).

find_characteristics(option([value(Ch)],[Ch])) :-
   rdfS(UID, ent:characteristic, Ch),
   rdfS(UID, ent:pdf, _PDF).

find_pages(option([value(UID)],[UID])) :-
   ref_m(UID, model, _Model),
   ref_m(UID, target_iframe, _Path).

find_categories(option([value(Cat)],[NS, ' :: ', Local])) :-
   ref_m(UID, category, Cat),
   ref_m(UID, model, _Model),
   atomic_list_concat([NS,Local], ':', Cat).
/*
model_elements(option([value(Short)],[Name])) :-
    rdf(ID, rdfs:'subClassOf', _),
    rdf_global_id(ent:Name, ID),
    context:concise_term(ID, Short).
*/

my_search_filter(Request) :-
   http_parameters(Request, [q(Q, [string])]),
   call_showing_messages(
       print_message(informational, format('~w query', Q)),
       []
			).
/*

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
*/

navigate(Request) :-
   % findall(Name, find_requirement_topics(Name), Ns),
   % sort(Ns, Names),
   findall(Character, find_characteristics(Character), Chs),  % use setof instead
   sort(Chs, Characteristics),
   findall(Page, find_pages(Page), Pgs),
   sort(Pgs, Pages),
   findall(Cat, find_categories(Cat), Cats),
   sort(Cats, Categories),
   %findall(Ent, model_elements(Ent), Ents),
   %sort(Ents, Elements),
   reply_html_page(cliopatria(default),
                   [title('Search Context')],
      [
       \(con_text:table_with_iframe_target(
                Request,
                [
                 h2( 'Search context patterns'),
                 ul([
                     li(p([\(con_text:gif(search)),
			   a([href='/context_model/navigate?render=render',
                           target=target_iframe],
                          'Search all PDF models'),
                        form([action('/context_model/navigate'),
                              target(target_iframe)],
                             [ select([name('characteristics')], Characteristics),
                               input([type(hidden),name(render),value(render)]),
                               input([type(submit)])  % place Name and value to generate other option
                             ]
                            )
                        ])
                       ),
		     /*
                     li(p([\(con_text:gif(search)),
			   'Search for requirements to establish workflow',
			   form([action('/context_ref_search/list_requirements')
                              , target=target_iframe
                              ],
                              [
                               select([name('name')], Names),
                               input([type('submit')])]
                             )])
                       ),
		     */
                     li(p([\(con_text:gif(search)),
			   'Search feature pages',
			   form([action('list_pages')
                              , target=target_iframe
                              ],
                              [
                               select([name('name')], Pages),
                               input([type('submit')])]
                             )])
                       ),
                     li(p([\(con_text:gif(search)),
			   'Search SWEET categories for models',
			   form([action('list_cats')
                              , target=target_iframe
                              ],
                              [
                               select([name('name')], Categories),
                               input([type('submit')])]
                             )])
                       )
/*
                     li(p([\(con_text:gif(search)),
			   'Search for model element definitions',
			   form([action('owl_result')
                              , target=target_iframe
                              ],
                              [
                               select([name('result')], Elements),
                               input([type('submit')])]
                             )])
                       ),

                     li(p([\(con_text:gif(search)),
			   'Search OWL',
		      \(con_text:form_ac(owl_result, target_iframe, owl, result))
                          ])
                       )
*/

                     ]),
		 \(con_text:render_iframe(render))


                ]
                                          )
        ),

       \simple_search_form([label('search literals'),
			    submit_handler(my_search_filter)
			   ])

      ]).



list_pages(Request) :-
   http_parameters(Request, [name(UID, [])]),
   print(user_error, [UID]),
   ref_m(UID, model, Model),
   ref_m(UID, target_iframe, Path),
   % atom_concat('.', Path, File),
   % read_file_to_codes(File, String, []),
   % string_to_atom(String, Text),
   reply_html_page(% cliopatria(default),
                   [title('Available Page'),
                    \(con_text:style)
                   ],
                   [
                     h1(['go to ',
			 a([href(Model),target('_parent')],
			   UID)]),
		    iframe([
			width('100%'),
			height('1000'),
			frameborder(0),
			src(Path)
			   ],
			   [])
		   ]
                   ).

available_pages(Cat, Model, UID) :-
   ref_m(UID, category, Cat),
   ref_m(UID, model, Model).

list_cats(Request) :-
   http_parameters(Request, [name(Cat, [])]),
   % ref_m(UID, category, Cat),
   % ref_m(UID, model, Model),
   findall(li(a([href(Model),
		 target('_parent')],
		UID)),
	   available_pages(Cat, Model, UID),
	   Models),
   context:create_global_term(Cat, Graph),
   % uri_normalized(Graph, Literal),
   % context:create_global_term(Literal, Graph),
   context:uri_index_link(Graph, 'definition', render, GraphLink),
   reply_html_page(% cliopatria(default),
                   [title('Available Page'),
                    \(con_text:style)
                   ],
                   [

		    p([\(con_text:gif(search)) ,
			' Models available for:']),
		    blockquote(h2([Cat, ' : ', GraphLink
				  ]
				 )
			      ),
		    ul(
			Models
		      ),
		    iframe([
			width('100%'),
			height('400'),
			frameborder(0),
			src(['/context_ref_search/graph?name=',Cat])
			   ],
			   [])
		   ]
		  ).





