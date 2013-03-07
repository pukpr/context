:- module(context_diffusive_decline, [
				% dd/5,
				diffusive_cumulative/4
			    ]).

/** <module> Diffusive Decline model
    * Ornstein-Uhlenbeck correction
    *
*/

:- use_module(context_math).

:- context:register(context_diffusive_decline:navigate).
:- context:register(context_diffusive_decline:plot).


diffusive_cumulative(Median, Max, X, Y) :-
    Y is Max/(1+sqrt(Median/(X+0.0000001))).

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

    Time range [0, 46]/1,
    production(Profile),
    sumlist(Profile, Max),
    Prod mapdot diffusive_cumulative(Median, Max) ~> Time,
    (
       Characteristic  = cumulative ->
         Cumulative accumulate Profile,
	 Data tuple Time + Cumulative + Prod,
         Heading = ['Time', 'Data', 'Model']
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
                     b('Max='),i(Max),
		     \(context_graphing:dygraph_native(lin,
						       Heading,
						       [X,XUnits], ['Production', YUnits],
						       Characteristic, Data))
                    ]
		  ).



% From EmmonsCoFB10152.pdf,  year 0 is actually year 1
production([0, 904,427,149,105,82,65,55,50,45,43,41,39,37,35,33,31,30,28,27,26,24,23,22,21,20,19,18,17,16,15,15,14,13,12,12,11,11,10,10,9,9,8,8,7,7,7]).


