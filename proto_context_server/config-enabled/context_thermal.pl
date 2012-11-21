:- module(context_thermal, [
			      thermalModel/5
			     ]).

:- use_module(context_math).

:- context:register(context_thermal:navigate).
:- context:register(context_thermal:plot).


navigate(_Request) :-
   collect_unit_options(ent:temperature, Hunits),
   collect_unit_options(ent:time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Thermal Environment')],
                   [\(con_text:table_with_iframe_target(
                                    target_iframe,
		     [
                      h1('Thermal Environment Models'),
                      p('Dispersed diffusion into an environmental heat sink'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  select([name('h_units')], Hunits),
			  input([type('text'),
				 name('limit'),
				 value('40')]), i(' <= margin'),
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


plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [number]),
                              t_units(TUnits, []),
                              h_units(HUnits, []),
                              evaluate(Characteristic, [])]),

    context_units:convert(0.01*day, T1*TUnits, T1),
    context_units:convert(100.0*day, T2*TUnits, T2),

    T range [T1, T2]^0.8*TUnits,
    (
       Characteristic = heat ->
	 Z mapdot thermalModel(10, 1.0, HUnits) ~> T
     ;
       Characteristic = cool ->
	 Z mapdot thermalModel(100, 1.0, HUnits) ~> T
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

thermalModel(Diff, X0, T*Time, Z*Thickness) :-
   context_units:convert(T*Time, T1*yr, T1),
   Z1 is Diff * T/(X0+sqrt(Diff*T)),
   context_units:convert(Z1*c, Z*Thickness, Z), !.
thermalModel(Diff, X0, Thickness, T*Time, Z) :-
   thermalModel(Diff, X0, T*Time, Z*Thickness).
