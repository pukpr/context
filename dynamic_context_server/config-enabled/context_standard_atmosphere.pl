:- module(context_standard_atmosphere, [ limit_value/3 ]).

/** <module> Standard atmosphere model
    * Extracted and interpolated from table
    *
*/

:- use_module(context_math).

:- context:register(context_standard_atmosphere:navigate).
:- context:register(context_standard_atmosphere:plot).

navigate(Request) :-
   collect_unit_options(length, Lunits), % elevation
   rdfS(ent:altitude, rdfs:comment, XDesc),

   reply_html_page(cliopatria(default),
                   [title('Title')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Standard Atmosphere Specifications'),
                      p('Interpolated standard atmsophere from lookup table'),
		      p(['Original data has ', XDesc]),
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
                                          ['Kinematic Viscosity', 'kv'],
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


limit_value(Value, X, 1.0) :- X =< Value, !.
limit_value(_, _, 0.0).




plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [number]),
                              l_units(LUnits, []),
                              evaluate(Characteristic, [])]),

    context_units:convert(1*kft, L*LUnits, L),

    rdf_global_term(ent:standard_atmosphere_table,STD),
    findall(Val, rdfR(STD, ent:altitude, Val), H),
    (
       Characteristic = sigma ->
         findall(Val, rdfR(STD, ent:sigma_density, Val), Vals),
         Y = 'sigma Density',
         rdfS(ent:sigma_density, rdfs:comment, YDesc),
         rdfS(ent:sigma_density, ent:unit, YUnits)
     ;
       Characteristic = dP ->
         findall(Val, rdfR(STD, ent:delta_pressure, Val), Vals),
         Y = 'delta Pressure',
         rdfS(ent:delta_pressure, rdfs:comment, YDesc),
         rdfS(ent:delta_pressure, ent:unit, YUnits)
    ;
       Characteristic = dT ->
         findall(Val, rdfR(STD, ent:delta_temperature, Val), Vals),
         Y = 'delta Temperature',
         rdfS(ent:delta_temperature, rdfs:comment, YDesc),
         rdfS(ent:delta_temperature, ent:unit, YUnits)
      ;
       Characteristic = temperature ->
         findall(Val, rdfR(STD, ent:temperature_rankine, Val), Vals),
         Y = 'Temperature',
         rdfS(ent:temperature_rankine, rdfs:comment, YDesc),
         rdfS(ent:temperature_rankine, ent:unit, YUnits)
      ;
       Characteristic = pressure ->
         findall(Val, rdfR(STD, ent:pressure_psi, Val), Vals),
         Y = 'Pressure',
         rdfS(ent:pressure_psi, rdfs:comment, YDesc),
         rdfS(ent:pressure_psi, ent:unit, YUnits)
      ;
       Characteristic = density ->
         findall(Val, rdfR(STD, ent:density_slugs, Val), Vals),
         Y = 'Density',
         rdfS(ent:density_slugs, rdfs:comment, YDesc),
         rdfS(ent:density_slugs, ent:unit, YUnits)
      ;
       Characteristic = sos ->
         findall(Val, rdfR(STD, ent:speed_of_sound, Val), Vals),
         Y = 'Speed of Sound',
         rdfS(ent:speed_of_sound, rdfs:comment, YDesc),
         rdfS(ent:speed_of_sound, ent:unit, YUnits)
      ;
       Characteristic = visc ->
         findall(Val, rdfR(STD, ent:viscosity_slugs, Val), Vals),
         Y = 'Viscosity',
         rdfS(ent:viscosity_slugs, rdfs:comment, YDesc),
         rdfS(ent:viscosity_slugs, ent:unit, YUnits)
      ;
       Characteristic = sos ->
         findall(Val, rdfR(STD, ent:speed_of_sound, Val), Vals),
         Y = 'Speed of Sound',
         rdfS(ent:speed_of_sound, rdfs:comment, YDesc),
         rdfS(ent:speed_of_sound, ent:unit, YUnits)
      ;
       Characteristic = kv ->
         findall(Val, rdfR(STD, ent:kinematic_viscosity, Val), Vals),
         Y = 'Kinematic Viscosity',
         rdfS(ent:kinematic_viscosity, rdfs:comment, YDesc),
         rdfS(ent:kinematic_viscosity, ent:unit, YUnits)
      ;
       Characteristic = kv_sos ->
         findall(Val, rdfR(STD, ent:ratio_kv_sos, Val), Vals),
         Y = 'Ratio of KV/SoS',
         rdfS(ent:ratio_kv_sos, rdfs:comment, YDesc),
         rdfS(ent:ratio_kv_sos, ent:unit, YUnits)
    ),
    H1 shrink H/Vals,
    Heights mapdot L .* H1,
    Step mapdot limit_value(Limit) ~> Heights,
    Limit_Curve mapdot Step * Vals,
    Data tuple Heights + Limit_Curve + Vals,
    Table tuple Heights + Vals,
    X = 'altitude',
    XUnits = LUnits, % 'km',
    (
    Kind = graph ->
    reply_html_page([title('Stanadard Atmosphere'),
                     \(con_text:style)],
                    [
		     % h3(XDesc),
		     % h3(YDesc),
		     \(context_graphing:dygraph_native(lin, [X, limit, Y],
						       [X,XUnits], [Y, YUnits],
						       YDesc, Data))
                    ]
		  )
    ;
    reply_html_page([title('Stanadard Atmosphere'),
                     \(con_text:style)],
                    [
		     % h3(XDesc),
		     h3(YDesc),
		     \(con_text:table_entries(XUnits, YUnits, Table))
                    ]
		  )
    ).








