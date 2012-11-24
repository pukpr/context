:- module(context_search, []).

/** <module> Context search main
    * General, free-form search
    * Semantic
*/

:- use_module(components(simple_search)).
:- use_module(components(messages)).

:- context:register(context_search:search).
:- context:register(context_search:my_search_filter).
:- context:register(context_search:list_pages).

find_requirement_topics(option([value(Name)],[Local])) :-
   rdf(Name, rdf:type, req:'Requirement'),
   rdf_split_url(_, Local, Name).

find_characteristics(option([value(Ch)],[Ch])) :-
   rdfS(UID, ent:characteristic, Ch),
   rdfS(UID, ent:pdf, _PDF).

find_pages(option([value(UID)],[UID])) :-
   ref_m(UID, model, _Model),
   ref_m(UID, target_iframe, _Path).


my_search_filter(Request) :-
   http_parameters(Request, [q(Q, [string])]),
   call_showing_messages(
       print_message(informational, format('~w query', Q)),
       []
			).

search(Request) :-
   findall(Name, find_requirement_topics(Name), Ns),
   sort(Ns, Names),
   findall(Character, find_characteristics(Character), Chs),  % use setof instead
   sort(Chs, Characteristics),
   findall(Page, find_pages(Page), Pgs),
   sort(Pgs, Pages),
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
                       ),
                     li(p([\(con_text:gif(search)),
			   'Search for pages',
			   form([action('list_pages')
                              , target=target_iframe
                              ],
                              [
                               select([name('name')], Pages),
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




