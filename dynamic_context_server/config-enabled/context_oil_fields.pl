:- module(context_oil_fields, []).

/** <module> DBpedia interface to oil fields
    *
    *
*/


:- context:register(context_oil_fields:navigate).
:- context:register(context_oil_fields:country).

:- use_module(context_math).
:- use_module(library(semweb/sparql_client)).
:- use_module(context_dbpedia).


%%   get_all_oil_regions(-List)
%
%    Get all oil regions as a list
get_all_oil_countries(List) :-
    findall(option([value(ID)],[Name]), available_countries(ID, Name), L),
    sort(L, List).

available_countries(ID, Name) :-
    rdf(ID, skos:broader, category:'Oil_fields_by_country'),
    context:concise_term(ID, Name).

country(Request) :-
    http_parameters(Request, [site(Country, [])]),
    format(atom(Format), 'select * where {?A dcterms:subject <~w> . }', [Country]),
    findall(li(Field),
            sparql_client:sparql_query(Format, row(Field), [host('dbpedia.org'), path('/sparql/')] ),
            Fields),
    reply_html_page([title(Country),
                     \(con_text:style)
                    ],
                    [
                     p(Country),
                     ul(Fields)
                    ]
                  ).


navigate(Request) :-
    get_all_oil_countries(List),
    reply_html_page(
        cliopatria(default),
        [title('Select region')
        ],
        [
         \(con_text:table_with_iframe_target(
                        Request,
                        [         form([action('/context_oil_fields/country'),
                                        target('target_iframe')],
                                       [
                                        select([name('site')], List),
                                        br([]),
                                        \(con_text:radio_toggles(
                                                       'measure',
                                                       [['Info', 'info'],
                                                        ['Graph', 'graph']
                                                       ])),
                                        input([type('submit'), value('Select Data Set')])
                                       ]),
                                  br([]),
                                  \(con_text:render_iframe(render))
                        ]
                   )
          )
        ]
                   ).


% ?- Format = 'select * where {?U skos:broader category:Oil_fields_by_country ; rdfs:label ?T }',
%    sparql_client:sparql_query(Format, row(I1,I2), [host('dbpedia.org'), path('/sparql/')] ).

%    Format = 'select * where {dbpedia:Orinoco_Belt dcterms:subject ?A . }',
%    sparql_client:sparql_query(Format, row(I1), [host('dbpedia.org'), path('/sparql/')] ).

%    Format = 'select * where {dbpedia:Orinoco_Belt dbpprop:discovery ?A . }'

%    Format = 'select * where {dbpedia:Orinoco_Belt dbpprop:estOilBbl ?A . }'
%
