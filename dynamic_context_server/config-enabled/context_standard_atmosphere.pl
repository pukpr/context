:- module(context_standard_atmosphere, [ ]).

:- use_module(context_math).

:- context:register(context_standard_atmosphere:navigate).
:- context:register(context_standard_atmosphere:plot).

navigate(Request) :-
   collect_unit_options(ent:length, Lunits), % elevation

   reply_html_page(cliopatria(default),
                   [title('Title')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Standard Atmosphere Specifications'),
                      p('Interpolated standard atmsophere from lookup table'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('l_units')], Lunits),
			  input([type('text'),
				 name('limit'),
				 value('3')]), i(' <= height selection'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['sigma Density', 'sigma'],
                                          ['delta Pressure', 'dP'],
                                          ['delta Temperature', 'dT'],
                                          ['Temperature (rankine)', 'temperature'],
                                          ['Pressure (psi)', 'pressure'],
                                          ['Density (slugs)', 'density'],
                                          ['Speed of Sound', 'sos'],
                                          ['Viscosity (slugs)', 'visc'],
                                          ['Kinematic Velocity', 'kv'],
                                          ['Ratio KV/SoS', 'kv_sos']
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('graph')]),
			  input([type('submit'), name(kind), value('table')])
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).

/*
standard_atmosphere(Parameter,X*_Units,Y) :-
   Y is sin(Parameter*X).
*/


plot(Request) :-
    http_parameters(Request, [kind(graph, []),
			      % limit(Limit, [number]),
                              l_units(LUnits, []),
                              evaluate(Characteristic, [])]),

    STD = 'ent:standard_atmosphere_table',
    findall(Val, rdfR(STD, ent:altitude, Val), H),
    (
       Characteristic = sigma ->
         findall(Val, rdfR(STD, ent:sigma_density, Val), Vals),
         Y = 'sigma Density',
         YUnits = density
     ;
       Characteristic = dP ->
         findall(Val, rdfR(STD, ent:delta_pressure, Val), Vals),
         Y = 'delta Pressure',
         YUnits = pressure
      ;
       Characteristic = dT ->
         findall(Val, rdfR(STD, ent:delta_temperature, Val), Vals),
         Y = 'delta Temperature',
         YUnits = c
      ;
       Characteristic = temperature ->
         findall(Val, rdfR(STD, ent:temperature_rankine, Val), Vals),
         Y = 'Temperature',
         YUnits = c
      ;
       Characteristic = pressure ->
         findall(Val, rdfR(STD, ent:pressure_psi, Val), Vals),
         Y = 'Pressure',
         YUnits = psi
      ;
       Characteristic = density ->
         findall(Val, rdfR(STD, ent:density_slugs, Val), Vals),
         Y = 'Density',
         YUnits = slugs
      ;
       Characteristic = sos ->
         findall(Val, rdfR(STD, ent:speed_of_sound, Val), Vals),
         Y = 'Speed of Sound',
         YUnits = 'm/s'
      ;
       Characteristic = visc ->
         findall(Val, rdfR(STD, ent:viscosity_slugs, Val), Vals),
         Y = 'Viscosity',
         YUnits = slugs
      ;
       Characteristic = sos ->
         findall(Val, rdfR(STD, ent:speed_of_sound, Val), Vals),
         Y = 'Speed of Sound',
         YUnits = 'm/s'
      ;
       Characteristic = kv ->
         findall(Val, rdfR(STD, ent:kinematic_velocity, Val), Vals),
         Y = 'Kinematic Velocity',
         YUnits = 'm/s'
      ;
       Characteristic = kv_sos ->
         findall(Val, rdfR(STD, ent:ratio_kv_sos, Val), Vals),
         Y = 'Ratio of KV/SoS',
         YUnits = ''
    ),
    H1 shrink H/Vals,
    Data tuple H1 + Vals,
    X = 'altitude',
    XUnits = LUnits, % 'km',
    reply_html_page([title('Stanadard Atmosphere'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin, [X, Y],
						       [X,XUnits], [Y, YUnits],
						       YUnits, Data))
                    ]
		  ).








