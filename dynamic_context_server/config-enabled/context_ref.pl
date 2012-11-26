:- module(context_ref, []).

/** <module> Reference section navigate
    * Through Zotero database
    * Tagged with SWEET terms
*/


:- context:register(context_ref:navigate).


navigate(Request) :-
   setof(Sweet, Sweet ^ (context_sweet_utils:find_sweets(Sweet)), Sweets),
   reply_html_page(cliopatria(default),
                   [title('Context References and Citations')],
      [
         \(con_text:table_with_iframe_target(
                        Request,
         [
          h2('Query ontology for references and citations'),
          ul([
              li(p([\(con_text:gif(ref)),
		    %a([href('/context_ref_search/cite?name=Confluence'),target(target_iframe)],
                    'Search for an author or tag indexed within the citation ontology',
                  form([action('/context_ref_search/cite'), target(target_iframe)],
                       [input([type('text'),name('name'),value('Confluence')]),
                        input([type('submit')])])
                   ])
              ),

            li(p([\(con_text:gif(ref)),
		  'Search for references based on SWEET terminology',
               form([action('/context_ref_search/search_sweet'),target(target_iframe)],
                   [
                    select([name('name')], Sweets),
                    input([type('submit')])]
                  )])
             )

             ]
            ),
	  \(con_text:render_iframe(render))

         ]))
       ]).

navigate(_) :-
   reply_html_page(% cliopatria(default),
                   [title('Home')],
                   [p('Failed to load page, likely because ontology not loaded.'),
                    p('This takes some time, but only needs to be done once.'),
                    \(con_text:button_link('Load Ontology',
					   '/context_sweet_utils/import_sweet',
					   'navigate'))]).





