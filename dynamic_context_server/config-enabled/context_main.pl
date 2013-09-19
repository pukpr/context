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
/*
:- http_handler('/html/images/sphere.pbm',
		http_reply_file(icons('favicon.ico'), []),
		[]).
:- http_handler('/favicon.ico',
		http_reply_file('/html/images/favicon.gif', []),
		[]).
*/

%%   server(+Port)
%
%    Instancing server for Cliopatria
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
cliopatria:menu_item(context/'/context_search/navigate', 'Search').
cliopatria:menu_item(context/'/context_browse/navigate', 'Browse').
cliopatria:menu_item(context/'/context_workflow/navigate', 'Work Flow').
cliopatria:menu_item(context/'/context_ref/navigate', 'References').
cliopatria:menu_item(context/'/context_require/navigate', 'User Req').
cliopatria:menu_item(context/'/context_map/navigate', 'Map').
cliopatria:menu_item(context/'/app', 'Features').
cliopatria:menu_item(context/'/context_resources/navigate', 'Resources').
cliopatria:menu_item(repository/'/context_sweet_utils/import_sweet?all=false',
                     'Load Context Data').
cliopatria:menu_item(repository/'/context_sweet_utils/import_sweet?all=true',
                     'Load All Ontology').
cliopatria:menu_item(repository/'https://www.zotero.org/user/login/',
                     'Zotero Ref Library').
cliopatria:menu_item(help/'/html/basic_terms.html', 'Vocabularies').
cliopatria:menu_item(help/'http://sweet.jpl.nasa.gov/', 'SWEET').
cliopatria:menu_item(help/'/context_demos/navigate', 'Eval Demos').
cliopatria:menu_item(help/'/context_main/acronym_list', 'Acronyms').
cliopatria:menu_item(places/'http://ContextEarth.com', 'Blog').
cliopatria:menu_item(places/'http://github.com/pukpr/context.git', 'Source').
cliopatria:menu_item(places/'/context_main/blog_feed', 'Comments').
cliopatria:menu_item(places/'/html/oscar.html', 'OSCAR').
cliopatria:menu_item(admin/'/context_main/run_unit_tests', 'Tests').
cliopatria:menu_item(query/'/terms#', 'Terms').
cliopatria:menu_item(query/'/context_query/navigate', 'Generic').

:- http_handler(root('.'), index_page, []).
:- http_handler(root(app), index_page, []).

:- context:register(context_main:run_unit_tests).
:- context:register(context_main:acronym_list).
:- context:register(context_main:blog_feed).

%%   nav_aids(Key, Path, Title)//
%
%    Inline generation of navigation aids
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
                target=render],
               img([src('/html/images/magnify-clip.png'),
		    title('more info')]))
                   ]
                  )
            ])
        ).

%%   index_page(+Request)
%
%    Main index page for context server
index_page(Request) :-
    reply_html_page(
        cliopatria(default),
        [title('Home')],
        [
          \(con_text:table_with_iframe_target(
                Request,
                [
                 \(context_select:icon_bar(none)),
                 h2('Dynamic Context Server - Features'),
                 ul([
                     \(context_main:nav_aids(require, '/context_require/navigate', 'Requirements')),
                     \(context_main:nav_aids(search, '/context_search/navigate', 'Search')), %
                     \(context_main:nav_aids(browse, '/context_browse/navigate', 'Browse')), %
                     \(context_main:nav_aids(workflow, '/context_workflow/navigate', 'Workflows')), %
                     \(context_main:nav_aids(ref, '/context_ref/navigate', 'References')),
                     \(context_main:nav_aids(resources,'/context_resources/navigate','Resources / Artifacts')),
                     \(context_main:nav_aids(map, '/context_map/navigate', 'Map View')), %
                     \(context_main:nav_aids(query, '/context_query/navigate', 'Generic Query')),
                     \(context_main:nav_aids(features, '/app', 'Features Home'))
		    ]),
		 br([]),
		 \(con_text:render_iframe(render))

                ]
                                    )
           )
        ]
                   ).


%%   run_unit_tests(+Request)
%
%    Run all unit tests
run_unit_tests(_Request) :-
    call_showing_messages(
    (   run_tests ),
        []
                         ).


%%   acronym_list(+Request)
%
%    Generate an acronym list table
acronym_list(_Request) :-
    findall(tr([th(Acronym),td(Definition)]),
            acronym_definition(Acronym,Definition),
            Defs),
    sort(Defs, Definitions),
    reply_html_page(
        cliopatria(default),
        [title('Acronyms')],
        [
         h1('Table of Acronyms'),
         table(Definitions)
        ]
                   ).



/*

Blog comments RSS Feed

*/

:- dynamic blog_content/1.

blog_content('').

blog_comments :-
	http_open([host('ContextEarth.com'),
		   path('/comments/feed')], S, []),
	load_xml_file(S,T),
	retract(blog_content(_)),
	assert(blog_content(T)).
blog_comments.

print_comments([]) --> !.
print_comments([element(item, _, List)|R]) -->
	{
	 member(element(title,_,Head), List),
	 member(element(pubDate,_,Date), List),
	 member(element(description,_,Contents), List)
	},
	html([h3([Head, ' ', i(Date)]),p(Contents)]),
	print_comments(R).
print_comments([_A|R]) -->
	print_comments(R).

get_blog_comments -->
	    {
	     blog_content([element(rss,
				   _Version,
				   [_,element(channel,[],
					      [_,element(title,_,Title)|
					       [_|Feed]])|_])]),
	     Feed=[_,_,_,_,_,_,_,_,_,_,_,_,_,_|T]
	    },
	    html(h1(Title)),
	    print_comments(T).


%%   blog_feed(+Request)
%
%    Generate a blog feed
blog_feed(_Request) :-
    blog_comments,
    reply_html_page(
        cliopatria(default),
        [title('Feed')],
        [
         \(get_blog_comments)
        ]
                   ).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%























