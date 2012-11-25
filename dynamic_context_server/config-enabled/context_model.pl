:- module(context_model, []).

/** <module> Context Model interface
   * RDF interface
*/

:- use_module(context).
:- use_module(context_math).
:- use_module(library('R')).
:- use_module(library(http/http_wrapper)).
:- use_module(context_r).

:- context:register(context_model:navigate).
:- context:register(context_model:apply).
:- context:register(context_model:data_generator).
:- context:register(context_model:code_generator).
:- context:register(context_model:data_service).

css(URL) -->
        html_post(css,
                  link([ type('text/css'),
                         rel('stylesheet'),
                         href(URL)
                       ])).

row(Characteristic,
    tr([td(Ch),
        td(a([href='apply?model='+Model],Model)),
        td(Location), td([Lat,'/',Lon]),
        td(Max), td(Min), td(Interval),
        td(Moments), td(PDF),
        td(Title),
        td(Xaxis)])
   ) :-
    (   Characteristic = all ->
        true
    ;
        Ch = Characteristic
    ),
    rdfS(U, ent:characteristic, Ch),
    rdfS(U, ent:model, Model),
    rdfR(U, ent:interval, Interval),
    rdf(U, ent:locale, Locale),
    rdfS(Locale, ent:title, Location),
    rdfR(Locale, ent:lat, Lat),
    rdfR(Locale, ent:lon, Lon),
    rdfR(U, ent:max_extent, Max),
    rdfR(U, ent:min_extent, Min),
    rdfS(U, ent:moments, Moments),
    rdfS(U, ent:title, Title),
    rdfS(U, ent:pdf, PDF),
    rdfS(U, ent:x_axis, Xaxis).


navigate(Request) :-
    http_parameters(Request, [characteristics(Ch,[default(all)])]),
    findall(Row, row(Ch,Row), Rows),
    reply_html_page(
                 [title('PDF'), \(con_text:style) ],
                 [table([border(1),
                         style('border:dotted')],
                        [tr([th('Characteristic'),
                             th('Model'),
                             th('Location'), th('Lat/Lon'),
                             th('Max'), th('Min'), th('Interval'),
                             th('Moments'), th('PDF'),
                             th('Title'),
                             th('Xaxis')])
                            | Rows])
                 ]
                   ).

distribution(pdf, 'Probability Density').
distribution(cdf, 'Cumulative Probability').

present_and_next_state(Present, Title, Next) :-
    distribution(Present, Title),
    distribution(_, Next),
    Title \= Next.


display(Model, Render, Dist, Scale*Xaxis) :-
    rdfS(U, ent:model, Model),
    rdfS(U, ent:characteristic, Characteristic),
    rdfR(U, ent:interval, Interval),
    rdf(U, ent:locale, Locale),
    rdfS(Locale, ent:title, Location),
    rdfR(Locale, ent:lat, Lat),
    rdfR(Locale, ent:lon, Lon),
    rdfR(U, ent:max_extent, Max),
    rdfR(U, ent:min_extent, Min),
    rdfL(U, ent:moments, Moments),
    rdfS(U, ent:title, Title),
    rdfV(U, ent:pdf, Distribution_Function,  ['Dist'=State]),
    rdfS(U, ent:x_axis, X_0),
    present_and_next_state(State, Dist, Flip),
    http_current_request(Request),
    context:create_url(Request, '/context_model/data_generator', [[model,Model]], Sampling_URL),

    X range [Min,Max]^Interval,
    Y mapdot Distribution_Function ~> X,
    XS mapdot Scale .* X,


    find_optional_units(Xaxis,Nunits,Dunits),
    reply_html_page([title(pdf), \(con_text:style)],
		    [
                    \(con_text:table_multiple_entries(
				     [[i('Characteristic'), i('Units'), i('Location'), i('Range'),i('Moments')]],
				     [[b(Characteristic), Xaxis, Location, '~w .. ~w ~w'-[Min,Max,X_0], \(space_list(Moments)) ]]
						     )
		       ),
		     form([action(apply), target(target_iframe)],
			 [
			  i('change '), select([name('n_unit')], Nunits), b(' / '),
			                select([name('d_unit')], Dunits),
			  % select([name('denominator')], Dunits),
			  input([type(hidden), name(model), value(Model)]),
			  input([type(hidden), name(render), value(Render)]),
			  input([type(hidden), name(dist), value(Dist)]),
			  input([type(submit), name(kind), value(units)])
			 ]
                          ),


                    br([]), % hr([]), br([]),
                    \(context_graphing:dygraph_plot(
                                           true,
                                           'X, Y',
                                           Xaxis,
                                           Dist,
                                           [Title, ' : ', State],
                                           [XS,Y] )),
                     br([]),
                    \(con_text:inline_button(
                                   \(con_text:button_link('Display Map',
					   '/context_map/view',
					   render,
					   [[lat, Lat],
					    [lon, Lon],
					    [title, Location]
					   ]))
                                            )
                     ),
                    \(con_text:inline_button(
                                   \(con_text:button_link('Generate Sampled Data',
					   'data_generator',
					   Render,
					   [[model, Model]
					   ]))
                                            )
                     ),
                    \(con_text:inline_button(
                                   \(con_text:button_link('Show Service Request',
					   'data_service',
					   Render,
					   [[url, Sampling_URL]
					   ]))
                                            )
                     ),
                    \(con_text:inline_button(
                                   \(con_text:button_link('Generate Source Code',
					   'code_generator',
					   Render,
					   [[model, Model]
					   ]))
                                            )
                     ),
                    \(con_text:inline_button(
                                   \(con_text:button_link('Create '+Flip,
					   'apply',
					   '_self',
					   [[model, Model],
                                            [render, Render],
					    [dist, Flip]
					   ]))
                                            )
                     )

                    ]).



get_sample(_*Dist, Sample) :-
    call(Dist, _, Sample), !.
get_sample(Dist, Sample) :-
    Dist = [_*D|_],                % This needs work to cover the other cases
    call(D, _, Sample).

data_generator(Request) :-
    http_parameters(Request, [model(Model,[])
			     ]
		   ),
    rdfS(U, ent:model, Model),
    rdfV(U, ent:pdf, Distribution_Function,  ['Dist'=State]),
    State = sample,
    get_sample(Distribution_Function, Sample),
    % call(Distribution_Function, _, Sample),
    reply_html_page([title(sample), \(con_text:style)],
                    [p(Sample)]).

code_generator(Request) :-
    http_parameters(Request, [model(Model,[])
			     ]
		   ),
    rdfS(U, ent:model, Model),
    rdfS(U, ent:pdf, Distribution_Function), % Need one for each
    reply_html_page([title(code), \(con_text:style)],
                    [p(['TBD from ',Distribution_Function] )]).

data_service(Request) :-
    http_parameters(Request, [url(URL,[])
			     ]
		   ),
    reply_html_page([title(code), \(con_text:style)],
                    [p('Copy and paste this URL:'),
		     p(b(URL))]).


apply(Request) :-
    http_parameters(Request, [model(Model,[]),
                              render(Render,[default(render)]),
			      dist(Dist,[default('Probability Density')]),
			      n_unit(Num_Units,[optional(true)]),
			      d_unit(Den_Units,[optional(true)])
			     ]
		   ),
    r_open_session,
    rdfS(U, ent:model, Model),
    rdfS(U, ent:x_axis, Xaxis),
    (	var(Num_Units) ->
        display(Model,Render,Dist,1.0*Xaxis)
    ;
	var(Den_Units) ->
	context_units:convert(1.0*Xaxis, To*Num_Units, To),
        display(Model,Render,Dist,To*Num_Units)
    ;
        atomic_list_concat([Num_Units,'/',Den_Units], Units),
        atom_to_term(Xaxis, Num/Den, []),
	context_units:convert(1.0*Num/Den, To*Num_Units/Den_Units, To),
        display(Model,Render,Dist,To*Units)
    ),
    r_close.


find_optional_units(Named_Unit,NList,DList) :-
   atom_to_term(Named_Unit, NUnit/DUnit, []),
   rdf_(UN, ent:unit, NUnit),
   rdf_(URI, ent:units, UN),!,
   context_units:collect_unit_options(URI, NList),
   rdf_(UD, ent:unit, DUnit),
   rdf_(URID, ent:units, UD),!,
   context_units:collect_unit_options(URID, DList).
find_optional_units(Named_Unit,NList,[]) :-
   atom_to_term(Named_Unit, Unit, []),
   rdf_(U, ent:unit, Unit),
   rdf_(URI, ent:units, U),!,
   context_units:collect_unit_options(URI, NList).
find_optional_units(_,[],[]).



