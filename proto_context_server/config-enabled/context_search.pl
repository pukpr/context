:- module(context_search, []).

/** <module> Context search main
    * General, free-form search
    * Semantic
*/

:- use_module(components(simple_search)).
:- use_module(components(messages)).

:- context:register(context_search:search).
:- context:register(context_search:my_search_filter).

find_requirement_topics(option([value(Name)],[Local])) :-
   rdf(Name, rdf:type, req:'Requirement'),
   rdf_split_url(_, Local, Name).

find_characteristics(option([value(Ch)],[Ch])) :-
   rdfS(UID, ent:characteristic, Ch),
   rdfS(UID, ent:pdf, _PDF).


my_search_filter(Request) :-
   http_parameters(Request, [q(Q, [string])]),
   call_showing_messages(
       print_message(informational, format('~w query', Q)),
       []
			).

search(Request) :-
   findall(Name, find_requirement_topics(Name), Names),
   findall(Character, find_characteristics(Character), Chs),  % use setof instead
   sort(Chs, Characteristics),
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
                     li(p([\(con_text:gif(search)),
			   'Search for requirements to establish workflow',
			   form([action('/context_ref_search/list_requirements')
                              , target=target_iframe
                              ],
                              [
                               select([name('name')], Names),
                               input([type('submit')])]
                             )])
                       )
                    ]),
		 \(con_text:render_iframe(render))


                ]
                                          )
        ),

       \simple_search_form([label('search literals'),
			    submit_handler(my_search_filter)
			   ])

      ]).




