:- module(context_main, []).

/** <module> Example Home Page and RDF calls, using clioptaria
    * Browses through examples and workflows
*/
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_head)).
:- use_module(cliopatria(hooks)).
:- use_module(components(messages)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%% Server rules for web processing %%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-  working_directory(W,W),
    http_handler(root(.), http_reply_from_files(W, []), [prefix]).

:- http_handler('/html/images/sphere.pbm',
		http_reply_file(icons('favicon.ico'), []),
		[]).

server(Port) :-
   http_server(http_dispatch, [port(Port)]).

cliopatria:server_address -->
        html([ address(class(footer),
                       [ a(href('http://entroplet.com'),
                           'dynamic context server')
                       ])
             ]).

% cliopatria:menu_item(places/'/app', 'Context Main').  % Return to main page
cliopatria:menu_popup_order(context, 10).
cliopatria:menu_item(context/'/context_search/search', 'Search').
cliopatria:menu_item(context/'/context_browse/browse', 'Browse').
cliopatria:menu_item(context/'/context_workflow/workflows', 'Work Flow').
cliopatria:menu_item(context/'/context_ref/navigate', 'References').
cliopatria:menu_item(context/'/context_require/navigate', 'User Req').
cliopatria:menu_item(context/'/context_map/search', 'Map').
cliopatria:menu_item(context/'/app', 'Features').
cliopatria:menu_item(context/'/context_resources/navigate', 'Resources').
cliopatria:menu_item(repository/'/context_sweet_utils/import_sweet?all=false',
                     'Load Context Data').
cliopatria:menu_item(repository/'/context_sweet_utils/import_sweet?all=true',
                     'Load All Ontology').
cliopatria:menu_item(repository/'https://www.zotero.org/user/login/',
                     'Zotero Ref Library').
cliopatria:menu_item(help/'/html/static_pages/gems/', 'Intro Slides').
cliopatria:menu_item(help/'/html/basic_terms.html', 'Vocabularies').
cliopatria:menu_item(help/'http://sweet.jpl.nasa.gov/', 'SWEET').
cliopatria:menu_item(help/'/context_demos/navigate', 'Example Demos').
cliopatria:menu_item(places/'https://babelfish.arc.nasa.gov/confluence/display/AVMPROJ/BAE', 'Wiki').
cliopatria:menu_item(places/'https://babelfish.arc.nasa.gov/trac/avm_performers/browser/context', 'Source').
cliopatria:menu_item(places/'https://babelfish.arc.nasa.gov/jira/browse/AVM', 'Tracking').
cliopatria:menu_item(places/'http://podaac.jpl.nasa.gov/', 'OSCAR').
cliopatria:menu_item(admin/'/context_main/run_unit_tests', 'Tests').
cliopatria:menu_item(query/'/context_ont_utils/find_ent_subjects', 'Subjects').
cliopatria:menu_item(query/'/context_ont_utils/find_ent_predicates', 'Predicates').
cliopatria:menu_item(query/'/context_ont_utils/find_ent_objects', 'Objects').
cliopatria:menu_item(query/'/context_file_reading/crawl', 'Models').



:- http_handler(root('.'), index_page, []).
:- http_handler(root(app), index_page, []).

% :- http_handler(root(browse), 'browse?', []).

:- context:register(context_main:pquery).
:- context:register(context_main:run_unit_tests).

nav_aids(Key, Path, Title) -->
    {
     (	 rdf_(Key, ent:narrative, Narration);
         Narration = 'empty doc'
     ),
   (	atom_chars(Narration, ['/'|_]) ->
        Doc = Narration
    ;
        Doc = '/con_text/info?info=~w'-Narration

   )
    },
    html(li([\(con_text:gif(Key)),
             a(href(Path),
               Title),
             small([
             ' - ',
             a([href(Doc),
                target=target_iframe],
               img([src('/html/images/magnify-clip.png'),
		    title('more info')]))
                   ]
                  )
            ])
        ).

index_page(Request) :-
    reply_html_page(
        cliopatria(default),
        [title('Home')],
        [
          \(context_select:icon_bar(none)),
          \(con_text:table_with_iframe_target(
                Request,
                [
                 h2('Dynamic Context Server - Features'),
                 ul([
                     \(context_main:nav_aids(require, '/context_require/navigate', 'Requirements')),
                     \(context_main:nav_aids(search, '/context_search/search', 'Search')),
                     \(context_main:nav_aids(browse, '/context_browse/browse', 'Browse')),
                     \(context_main:nav_aids(workflow, '/context_workflow/workflows', 'Workflows')),
                     \(context_main:nav_aids(ref, '/context_ref/navigate', 'References')),
                     \(context_main:nav_aids(resources,'/context_resources/navigate','Resources / Artifacts')),
                     \(context_main:nav_aids(map, '/context_map/search', 'Map View')),
                     \(context_main:nav_aids(features, '/app', 'Features Home'))
		    ]),
		 br([]),
		 h2('Other links'),
		 ul([
		     li([a(href('context_demos/navigate'),
                           'Navigate to other ontology demos')]),
		     li([a([href('context_map/navigate?lat=48.786&lon=9.235&title="Mercedes-Benz TT"'),
			    target(target_iframe)],
                           'Display map')]),
		     li(a([href('/sparql/?query=select * where{?s relaSci:hasNumericValue ?o}')],
                          'SPARQL query syntax')),
		     li([
                         \(con_text:form('/context_main/pquery',
				 target_iframe,
				 [[input,'t(Subject,dc:title,Object)']]
				)),
                         'prolog query'
                        ]
		       ),
		     li( [
                         \(con_text:form('/context_water/density_test',
				 target_iframe,
				 [[input,'20', 6],
				  [iunits,'c', 6],
				  [ounits,'g/cm^3', 6]]
				)),
                         'units conversion'
                         ])
                    ])

                ]
                                    )
           )
        ]
                   ).

t(S,P,O) :-
    rdf_global_term(P, T),
    rdf(S,T,O).


pquery(Request) :-
    http_parameters(Request, [input(Input, [])]),
    atom_to_term(Input, Terms, Var),
    call(Terms),
    reply_html_page(
        [title('Prolog query')],
        [p(\(con_text:flist(Var)))]
                   ).

run_unit_tests(_Request) :-
    call_showing_messages(
    (   run_tests ),
        []
                         ).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%























