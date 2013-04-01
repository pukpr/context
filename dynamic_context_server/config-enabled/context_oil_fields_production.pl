:- module(context_oil_fields_production, [
			    ]).

:- use_module(context_math).

:- context:register(context_oil_fields_production:navigate).
:- context:register(context_oil_fields_production:plot).

%%   oil_field(+ID, -Name)
%
%    Local RDF data
oil_field(ID, Name) :-
    rdf(ID, rdf:type, ent:oil_field),
    rdf(ID, ent:name, literal(Name)).

%%   get_all_oil_fields(-List)
%
%    Get all oil regions as a list
get_all_oil_fields(List) :-
    findall(option([value(ID)],[Name]), oil_field(ID, Name), L),
    sort(L, List).


navigate(Request) :-
   collect_unit_options(volume, Lunits),
   get_all_oil_fields(List),

   reply_html_page(cliopatria(default),
                   [title('Oil Field Production')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Oil Field Production Level'),
                      p('select field'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('l_units')], Lunits),
			  input([type('text'),
				 name('limit'),
				 value('0')]), i(' <= margin'),
			  br([]),
			  select([name('data_set')], List),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Name1', 'id1'],
                                          ['Name2', 'id2'],
                                          ['Name3', 'id3']
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('lin')]),
			  input([type('submit'), name(kind), value('log')])
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).



plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(_Limit, [number]),
                              l_units(LUnits, []),
			      data_set(Field, []),
                              evaluate(_Characteristic, [default(id1)])]),

    context:rdfL(Field, ent:data, Prods),
    oil_field(Field, Name),
    context:rdfS(Field, ent:total, Cumulative),
    Years ordinal Prods,
    Calendar mapdot 1975 .+ Years,
    % H range [0.1, 10.0]^0.9*LUnits,
    Data tuple Calendar + Prods,
    X = 'Time',
    Y = 'Production',
    XUnits = 'year',
    YUnits = LUnits,
    reply_html_page([title('Production'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(Kind, [X, Y],
						       [X,XUnits], [Y, YUnits],
						       ['Production Level : ', Name],
                                                       Data)),
                     br([]),
                     i('cumulative='),
                     b(Cumulative)
                    ]
		  ).

