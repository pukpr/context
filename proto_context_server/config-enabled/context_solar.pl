:- module(context_solar, []).

:- context:register(context_solar:navigate).
:- context:register(context_solar:plot).

:- use_module(context_math).

list_solar_models(Target) -->
/*
    {findall(option([value(Name)],[Name]),
                    (rdf_(ent:ar7038_climate, ent:climate_design_type, ID),
                     rdf_(ID, ent:name, Name)), Selects)},
*/
    html(
        form([action(plot), target(Target)],
			 [
			  % select([name('solar_type')], [value(bb)],[bb]),
			  \(con_text:radio_toggles('evaluate',
					 [['by wavelength', 'wavelength'],
					 ['by wavenumber', 'wavenumber'],
					 ['by frequency', 'frequency']])),
			  /* \(con_text:radio_box_input_two(
					 'evaluate',
					 ['by wavelength', 'wavelength'],
					 ['by wavenumber', 'wavenumber'],
					 ['by frequency', 'frequency']
                                                        )),
			  */
			  br([]),
			  input([type('submit'), name(kind), value('Plot')])
			 ]
            )
        ).


navigate(Request) :-
   % rdf_(ent:ar7038_spec, ent:description, Description),
   reply_html_page(cliopatria(default),
                   [title('Solar Insolation')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Solar radiation specifications'),
                      % p(Description),
                      ul(li(['Select solar type',
                             \(context_solar:list_solar_models(target_iframe))
                            ]
                           ))
                     ,
                      br([]),
		      \(con_text:render_iframe(render))
                     ]
                                         ))
     ]
		  ).


plot(Request) :-
    http_parameters(Request, [kind(_Kind, []),
                              % solar_type(Solar_Name, []),
                              evaluate(Characteristic, [])]),

    % context:create_global_term(Characteristic, Ch),
    % atom_to_term(Characteristic, Ch, []),
    (	Characteristic = wavelength ->
        WL range [0.1, 20.0]/0.1*micron,
        Pr mapdot plancks_law(micron, 5000*k) ~> WL
    ;
        Characteristic = wavenumber ->
        WL range [0.1, 20.0]/0.1*micron,
        Pr mapdot plancks_law_wavenumber(micron, 5000*k) ~> WL
    ;
        WL range [0.1, 20.0]/0.1*micron,
        Pr mapdot plancks_law_frequency(micron, 5000*k) ~> WL
    ),
    Data tuple WL + Pr,
    reply_html_page([title('Solar'),
                     \(con_text:style)],
                    [
                     \(context_graphing:dygraph_native(log, [micron,'P'], 'wavelength', 'Intensity',
                                                       Characteristic,
                                                       Data))
/*
                     \(con_text:inline_button(
		       \(con_text:button_link(
				      'Link to specifications',
                                      Path+SpecPage,
				      % '/ref/AR-70-38.html#p02-2',
				      render,
				      []))
				)
                      ),
*/

                    ]
		  ).






