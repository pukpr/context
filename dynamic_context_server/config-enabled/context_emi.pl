:- module(context_emi, [
			    %  fxn1/3,
			    %  fxn2/3,
			    %  fxn3/3
			    ]).

:- use_module(context_math).

:- context:register(context_emi:navigate).
:- context:register(context_emi:plot).

% /ref/cable_EMI_susceptibility.pdf#nameddest=CS_BCI

navigate(Request) :-

   reply_html_page(cliopatria(default),
                   [title('general EMI models')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Links to EMI models'),
                      p('Linked specifications provided for EMI models'),

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












