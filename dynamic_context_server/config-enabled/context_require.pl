:- module(context_require, []).

/** <module> Context requirements
    * Generic semantics for linking requirements to models
    *
*/

:- context:register(context_require:navigate).

find_requirement_topics(option([value(Name)],[Local])) :-
   rdf(Name, rdf:type, req:'Requirement'),
   rdf_split_url(_, Local, Name).

navigate(Request) :-
   findall(Name, find_requirement_topics(Name), Nlist),
   sort(Nlist, Names),
   reply_html_page(cliopatria(default),
                   [title('Requirements')],
      [
       \(con_text:table_with_iframe_lower_target(
                Request,
                [
                 h2( 'Linking requirements to context models'),
                 p(['Phrases such as "wind speed" or "inclined terrain" in project requirements',
                    ' map to applicable context models.']),
                 ul([
                     li(p([\(con_text:gif(require)),
			   'Search for requirements to establish workflow',
			   form([action('/context_ref_search/list_requirements'),
                                 target(target_iframe)
                              ],
                              [
                               select([name('name')], Names),
                               input([type('submit')])]
                             )])
                       )
                   ])
		 % \(con_text:render_iframe(render))

                ]
                                          )
        )

      ]).




