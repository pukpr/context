:- module(context_diffusive_decline, [
				% dd/5,
				diffusive_cumulative/3
			    ]).

/** <module> Diffusive Decline model
    * Ornstein-Uhlenbeck correction
    *
*/

:- use_module(context_math).

:- context:register(context_diffusive_decline:navigate).
:- context:register(context_diffusive_decline:plot).


diffusive_cumulative(Median, X, Y) :-
    Y is 1.0/(1+sqrt(Median/(X+0.0000001))).

%%   navigate(+Request)
%
%    Dynamic page to shock model
navigate(Request) :-
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Diffusive Decline model'),
	           script([type('text/javascript'),src('/html/js/submit.js')], [])
                   ],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Plot a diffusive decline model'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  input([type('text'),
				 name('median'),
				 value('6')]), i(' <= median'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Cumulative', 'cumulative'],
                                          ['Ornstein-Uhlenbeck', 'ou'],
					  ['Yearly', 'discovery'],
                                          ['O-U Yearly', 'ou_yearly']
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('plot'),
                                 onclick('subm(this.form,"target_iframe");')]),
			  input([type('submit'), name(kind), value('table'),
                                 onclick('subm(this.form,"render");')])
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).



%%   plot(+Request)
%
%    Plot the shock model profile
plot(Request) :-
    http_parameters(Request, [kind(table, []),
			      median(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(_Table, [])]),
    Time range [0, 200]/1,
    % shocks(Shocks),
    % interpolate(Time, Shocks, Sh),
    Data tuple Time +

    Time,

    reply_html_page([title('Shock Extraction'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin, [date, rate],
						       ['Date', year], ['Rate', fraction],
						       'extraction rate', Data))
                    ]
		  ).



plot(Request) :-
    http_parameters(Request, [kind(plot, []),
			      median(Median, [number]),
                              t_units(_TUnits, []),
                              evaluate(Characteristic, [default(cumulative)])]),

    Time range [0, 60]/1,
    % production(Profile),
    Prod mapdot diffusive_cumulative(Median) ~> Time,
    (
       Characteristic  = cumulative ->
	 Data tuple Time + Prod,
         Heading = ['Time', 'Model']
    ;
       Characteristic = ou  ->
	 Data tuple Time + Prod,
         Heading = ['Time', 'Model']
    ;
       Characteristic = yearly  ->
	 Data tuple Time + Prod,
         Heading = ['Time', 'Model']
    ;
       Characteristic = ou_yearly  ->
	 Data tuple Time + Prod,
         Heading = ['Time', 'Model']
    ),
    X = 'Date',
    XUnits = ' year',
    YUnits = ' fractional decline',
    reply_html_page([title('Shock Profile'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin,
						       Heading,
						       [X,XUnits], ['Production', YUnits],
						       Characteristic, Data))
                    ]
		  ).


