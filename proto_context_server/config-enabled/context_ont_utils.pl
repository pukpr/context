:- module(context_ont_utils, []).

/** <module> Ontology utilities
    * Through Zotero database
    * Tagged with SWEET terms
*/
:- use_module(components(messages)).


:- context:register(context_ont_utils:restart).
:- context:register(context_ont_utils:any_model_index).
:- context:register(context_ont_utils:load_data).
:- context:register(context_ont_utils:navigate).
:- context:register(context_ont_utils:find_ent_objects).
:- context:register(context_ont_utils:find_ent_subjects).
:- context:register(context_ont_utils:find_ent_predicates).



navigate(_) :-
   reply_html_page(
       cliopatria(default),
       [title('Home')],
       [ h2('Query Ontology examples'),
         \(con_text:table_with_iframe_target(
            target_iframe,
            ul([
                li([a([href('/context_sweet_utils/import_sweet?all=false'),
                      target(target_iframe)],
                      'Import Ontology'),
                    i(' (only needed once, maintains persistent store) '),
                    a([href('/context_sweet_utils/import_sweet?all=true'),
                      target(target_iframe)],
                      '[all triples]')]),
                li(a(href('load_data?file=../../../examples/model5.rdf'),
                     'Load data from a file')),
                li(a(href('load_data?url=http://wcsn262:5001/examples/model5.rdf'),
                     'Load data from an arbitrary web-served file')),

                li([img(src('/icons/doc.png')), h2('Other links')]),
                li(a(href('/home'), 'Server Admin Home')),
                li(a(href('http://sweet.jpl.nasa.gov'), 'SWEET Home')),
                li(a(href('load_data?file=../../../library/triples/combined_ontology.rdf'),
                     'Load combined component ontology')),
                li(a([href('restart'),
                      target(target_iframe)], 'Try to restart server'))
               ]
              )
                                 )
          )
       ]
                  ).


% also load http://purl.org/net/biblio
%           http://xmlns.com/foaf/spec/index.rdf

% %%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%% Library terms %%%
% %%%%%%%%%%%%%%%%%%%%%%%%

load_resource(File) :-
   rdf_load(File).

load_data(Request) :-   %% File name version
   http_parameters(Request, [file(Name, [ optional(true) ])]),
   nonvar(Name),
   load_resource(Name),
   reply_html_page(cliopatria(default), title('File loader'), [p(Name), p(' load completed.')]).

load_data(Request) :-   %% URL version
   http_parameters(Request, [url(URL, [ optional(true) ])]),
   nonvar(URL),
   % load_resource(URL),
   rdf_load(URL, [format('xml')]),
   %% add something here
   reply_html_page(cliopatria(default), title('URL loader'), [p(URL), p(' load completed.')]).

% command line only
print_all_subjects :-
   rdf(Subject, rdf:type, Sub),
   rdf_reachable(Sub, rdf:type, owl:'Class'),
   print(Subject), print(' '), print(Sub), nl,
   fail.

any_model_index(_Request) :-
   reply_html_page(cliopatria(default), title('Any Model'),
                   [ p('not yet implemented')]).


write_object(Name) :-
    print_message(informational, format('~w', Name)).

find_ent_predicates(_Request) :-
 call_showing_messages(
    (
    findall(P,
            (
             rdf(_A,B,_C),
             rdf_global_object(ent:P, B)
            ),
            List),
    sort(List,L),
    maplist(write_object, L)
    ),
        []
                         ).


find_ent_subjects(_Request)  :-
 call_showing_messages(
    (
    findall(P,
            (
             rdf(A,_B,_C),
             rdf_global_object(ent:P, A)
            ),
            List),
    sort(List,L),
    maplist(write_object, L)
    ),
        []
                         ).


find_ent_objects(_Request)  :-
 call_showing_messages(
    (
    findall(P,
            (
             rdf(_A,_B,C),
             rdf_global_object(ent:P, C)
            ),
            List),
    sort(List,L),
    maplist(write_object, L)
    ),
        []
                         ).




% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%% I wish this worked to reboot %%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

restart(_Request) :- halt.

% gendot :-
%    rdf_(A,B,C),
%    print(A), print(' -> '), print(C),
%    print(' [label="'), print(B), print('"];'), nl,
%    fail.








