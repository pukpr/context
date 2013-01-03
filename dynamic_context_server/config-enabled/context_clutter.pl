:- module(context_clutter, [
			      clutter_integrate/3,
			      other_fxn/3
			    ]).

/** <module> Model of EMI clutter
    * PDF of clutter
    * Integration model for GPS cold start
    *
*/

:- use_module(context_math).
:- use_module(context_units).

:- context:register(context_clutter:navigate).
:- context:register(context_clutter:plot).

%%   navigate(+Request)
%
%    Dynamic page to EMI clutter models
navigate(Request) :-
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Clutter model')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Time to cold-start with clutter'),
                      p('In noisy environment noise is integrated'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  input([type('text'),
				 name('limit'),
				 value('300')]), i(' <= time deadline'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Integrate', 'integrate']
                                           % ,['Name3', 'id3']
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


%%   clutter_integrate(+Parameter,+X,+Y)
%
%    Intgerate the stochastic clutter model
clutter_integrate(Parameter,X,Y) :-
   Y is Parameter*sqrt(X/(1-X)).
other_fxn(Parameter,X,Y) :-
   Y is Parameter*X.


%%   plot(+Request) 
%
%    Plot clutter model
plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [number]),
                              t_units(TUnits, []),
                              evaluate(Characteristic, [default(integrate)])]),

    Prob range [0.01, 0.999]^0.99,  % *TUnits,
    context_units:convert(62.0*s, Scale*TUnits, Scale),
    (
       Characteristic = integrate ->
	 Time mapdot clutter_integrate(Scale) ~> Prob
     ;
       Characteristic = others ->
	 Time mapdot other_fxn(Limit) ~> Prob
    ),
    Lim mapdot Limit ~> Prob,
    Data tuple Prob + Time + Lim,
    X = 'probability of occuring',
    Y = 'time to cold start',
    XUnits = 'normalized to one',
    YUnits = TUnits,
    reply_html_page([title('GPS Cold Start acquire'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(Kind, [X, Y, 'deadline for occurrence'],
						       [X,XUnits], [Y, YUnits],
						       'GPS Cold Start acquire', Data))
                    ]
		  ).

