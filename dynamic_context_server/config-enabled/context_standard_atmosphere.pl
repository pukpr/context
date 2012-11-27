:- module(context_standard_atmosphere, [
			    ]).

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

/*
fxn1(Parameter,X*_Units,Y) :-
   Y is sin(Parameter*X).
fxn2(Parameter,X*_Units,Y) :-
   exp(Parameter, X, Y).
fxn3(Parameter,X*_Units,Y) :-
   Y is Parameter*X.
*/

plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [number]),
                              l_units(LUnits, []),
                              evaluate(Characteristic, [])]),

    H range [0.1, 10.0]^0.9*LUnits,
    (
       Characteristic = id1 ->
	 Z mapdot fxn1(Limit) ~> H
     ;
       Characteristic = id2 ->
	 Z mapdot fxn2(Limit) ~> H
      ;
       Characteristic = id3 ->
	 Z mapdot fxn3(Limit) ~> H
    ),
    Data tuple H + Z,
    X = 'xname',
    Y = 'yname',
    XUnits = 'units',
    YUnits = LUnits,
    reply_html_page([title('PlotTitle'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(Kind, [X, Y],
						       [X,XUnits], [Y, YUnits],
						       'plot_title', Data))
                    ]
		  ).








