:- module(context_thermal, [
			      thermalModelHeat/5,
			      thermalModelCool/5
			     ]).

:- use_module(context_math).

:- context:register(context_thermal:navigate).
:- context:register(context_thermal:plot).


navigate(Request) :-
   collect_unit_options(temperature, Hunits),
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Thermal Environment')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Thermal Environment Models'),
                      p('Dispersed diffusion into an environmental heat sink'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  select([name('h_units')], Hunits),
			  input([type('text'),
				 name('limit'),
				 value('0.5')]), i(' <= margin'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Heat', 'heat'],
                                          ['Cool', 'cool']
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

thermalModelHeat(Diff, X0, T*Time, Z*Temperature) :-
   context_units:convert(T*Time, T1*day, T1),
   thermal_dispersion(Diff, X0, T1, Z1),
   context_units:convert(Z1*c, Z*Temperature, Z), !.
thermalModelHeat(Diff, X0, Temperature, T*Time, Z) :-
   thermalModelHeat(Diff, X0, T*Time, Z*Temperature).


thermalModelCool(Diff, X0, T*Time, Z*Temperature) :-
   context_units:convert(T*Time, T1*day, T1),
   thermal_dispersion(Diff, X0, T1, Z1),
   Z2 is X0-Z1,
   context_units:convert(Z2*c, Z*Temperature, Z), !.
thermalModelCool(Diff, X0, Temperature, T*Time, Z) :-
   thermalModelCool(Diff, X0, T*Time, Z*Temperature).

/*
thermalModelCool(Diff, X0, T*Time, Z*Thickness) :-
   context_units:convert(T*Time, T1*day, T1),
   Z1 is Diff * T/(X0+sqrt(Diff*T)),
   context_units:convert(Z1*c, Z*Thickness, Z), !.
thermalModelCool(Diff, X0, Thickness, T*Time, Z) :-
   thermalModelCool(Diff, X0, T*Time, Z*Thickness).
*/

plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [number]),
                              t_units(TUnits, []),
                              h_units(HUnits, []),
                              evaluate(Characteristic, [default(heat)])]),

    context_units:convert(0.0000001*day, T1*TUnits, T1),
    context_units:convert(5.0*day, T2*TUnits, T2),

    T range [T1, T2]^0.9*TUnits,
    (
       Characteristic = heat ->
	 Z mapdot thermalModelHeat(1.0, 1.0, HUnits) ~> T
     ;
       Characteristic = cool ->
	 Z mapdot thermalModelCool(1.0, 1.0, HUnits) ~> T
    ),
    Top mapdot Limit ~> T,
    Data tuple T + Z + Top,
    X = 'Time',
    Y = 'Temperature',
    ZZ = 'Limit',
    XUnits = TUnits,
    YUnits = HUnits,
    reply_html_page([title('Thermal Models'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(Kind, [X, Y, ZZ],
						       [X,XUnits], [Y, YUnits],
						       'Temperature vs Time', Data)),
                     \(con_text:inline_button(
		       \(con_text:button_link(
				      'Link to references',
				      '/context_ref_search/search_sweet',
				      render,
				      [[name,'propConductivity:ThermalConductivity']]))
				)
                      )

                    ]
		  ).

