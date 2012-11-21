:- module(context_clutter, [
			      clutter_integrate/3,
			      other_fxn/3
			    ]).

:- use_module(context_math).
:- use_module(context_units).

:- context:register(context_clutter:navigate).
:- context:register(context_clutter:plot).

navigate(_Request) :-
   collect_unit_options(ent:time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Clutter model')],
                   [\(con_text:table_with_iframe_target(
                                    target_iframe,
		     [
                      h1('Time to cold-start with clutter'),
                      p('In noisy environment noise is integrated'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  input([type('text'),
				 name('limit'),
				 value('3')]), i(' <= time limit'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Integrate', 'integrate'],
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


clutter_integrate(Parameter,X*_Units,Y) :-
   Y is sin(Parameter*X).
other_fxn(Parameter,X*_Units,Y) :-
   Y is Parameter*X.


plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [number]),
                              t_units(TUnits, []),
                              evaluate(Characteristic, [])]),

    H range [0.1, 10.0]^0.9*TUnits,
    (
       Characteristic = integrate ->
	 Z mapdot clutter_integrate(Limit) ~> H
     ;
       Characteristic = id3 ->
	 Z mapdot other_fxn(Limit) ~> H
    ),
    Data tuple H + Z,
    X = 'xname',
    Y = 'yname',
    XUnits = 'units',
    YUnits = TUnits,
    reply_html_page([title('PlotTitle'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(Kind, [X, Y],
						       [X,XUnits], [Y, YUnits],
						       'plot_title', Data))
                    ]
		  ).

