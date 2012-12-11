:- module(context_solar, []).

:- context:register(context_solar:navigate).
:- context:register(context_solar:plot).

:- use_module(context_math).

list_solar_models(Target) -->
	{
   collect_unit_options(length, Lunits) % elevation
	},
    html(
        form([action(plot), target(Target)],
			 [
			  % select([name('solar_type')], [value(bb)],[bb]),
			  \(con_text:radio_toggles('evaluate',
					 [['by wavelength', 'wavelength'],
					 ['by wavenumber', 'wavenumber']
					 % ['by frequency', 'frequency']
					 ])),
			  /* \(con_text:radio_box_input_two(
					 'evaluate',
					 ['by wavelength', 'wavelength'],
					 ['by wavenumber', 'wavenumber'],
					 ['by frequency', 'frequency']
                                                        )),
			  */
			  br([]),
			  input([type('text'),
				 name('temperature'),
				 value('6000'),
				 size(5)]), i(' <= source temperature (K)'),
			  br([]),
			  select([name('l_units')], Lunits),
			  input([type('submit'), name(kind), value('Plot')])

			 ]
            )
        ).


navigate(Request) :-
   reply_html_page(cliopatria(default),
                   [title('Solar Insolation')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Black Body / Solar spectral radiation'),
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
                              temperature(Temperature, [number]),
                              l_units(LUnits, []),
                              evaluate(Characteristic, [default(wavelength)])]),

    % context:create_global_term(Characteristic, Ch),
    % atom_to_term(Characteristic, Ch, []),

    (	Characteristic = wavelength ->
	context_units:convert(1*m, L*LUnits, L),
        Low is 1e-7*L,
	High is 2e-6*L,
	Interval is Low/10,
        WL range [Low, High]/Interval*LUnits,
        Pr mapdot plancks_law(LUnits, Temperature*k) ~> WL,
	Units = LUnits,
	Intensity = 'Intensity (w/m^2/m/steradian)'
    ;
        Characteristic = wavenumber ->
        context_units:convert(1/m, L/LUnits, L),
        Low is (1/20e-6)*L,
	High is (1/1e-7)*L,
	Interval is Low/10,
        WL range [Low, High]/Interval*LUnits,
        Pr mapdot plancks_law_wavenumber(LUnits, Temperature*k) ~> WL,
	format(atom(Units), '1/~w', [LUnits]),
	Intensity = 'Intensity (w/m^2/(1/m)/steradian)'
    /*
    ;
        WL range [0.1, 20.0]/0.1*LUnits,
        Pr mapdot plancks_law_frequency(LUnits, Temperature*k) ~> WL
    */
    ),
    Data tuple WL + Pr,
    reply_html_page([title('Solar'),
                     \(con_text:style)],
                    [
                     \(context_graphing:dygraph_native(lin, [LUnits,'P'],
						       [Characteristic, Units],
						       Intensity,
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






