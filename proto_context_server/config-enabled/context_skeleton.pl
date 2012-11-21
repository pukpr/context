:- module(context_skeleton, [
			      fxn1/3,
			      fxn2/3,
			      fxn3/3
			    ]).

:- use_module(context_math).

:- context:register(context_skeleton:navigate).
:- context:register(context_skeleton:plot).

navigate(_Request) :-
   collect_unit_options(ent:length, Lunits),

   reply_html_page(cliopatria(default),
                   [title('Title')],
                   [\(con_text:table_with_iframe_target(
                                    target_iframe,
		     [
                      h1('HeaderText'),
                      p('Explanation'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('l_units')], Lunits),
			  input([type('text'),
				 name('limit'),
				 value('3')]), i(' <= margin'),
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


fxn1(Parameter,X*_Units,Y) :-
   Y is sin(Parameter*X).
fxn2(Parameter,X*_Units,Y) :-
   exp(Parameter, X, Y).
fxn3(Parameter,X*_Units,Y) :-
   Y is Parameter*X.


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

