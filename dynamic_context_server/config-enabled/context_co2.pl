:- module(context_co2, [
				dd/5,
				exp_lag/3
			    ]).

/** <module> CO2 adjustment model
    * From
    *
*/

:- use_module(context_math).

:- context:register(context_co2:navigate).
:- context:register(context_co2:plot).


exp_lag(Mean, X, Y) :-
    Y = 1.0/Mean * (exp(-X/Mean)+exp(-(X+1.0)/Mean))/2.


dd(Exp, C, T, Year, Result) :-
    Result is C/(1+(T/(Year+0.01))^Exp).

%%   navigate(+Request)
%
%    Dynamic page to shock model
navigate(Request) :-
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('CO2 emissions model'),
	           script([type('text/javascript'),src('/html/js/submit.js')], [])
                   ],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Plot CO2 emissions'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  input([type('text'),
				 name('limit'),
				 value('0')]), i(' <= margin'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['CO2', 'co2'],
                                          ['Carbon', 'carbon'],
					  ['Both', 'both']
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


production([1,2,3]).
shocks([1,2,3]).



%%   plot(+Request)
%
%    Plot the shock model profile
plot(Request) :-
    http_parameters(Request, [kind(table, []),
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(_Table, [])]),
    Time range [0, 200]/1,
    shocks(Shocks),
    interpolate(Time, Shocks, Sh),
    Date mapdot 1850 .+ Time,
    Data tuple Date + Sh,

    reply_html_page([title('CO2'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin, [date, rate],
						       ['Date', year], ['Rate', fraction],
						       'extraction rate', Data))
                    ]
		  ).



plot(Request) :-
    http_parameters(Request, [kind(plot, []),
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(Characteristic, [default(shocked)])]),

    Time range [0, 300]/1,
    Date mapdot 1850 .+ Time,
    % Margin mapdot Limit ~> Time,
    production(Profile),
    DD mapdot dd(8, 2200, 114) ~> Time,
    Discoveries derivative DD/Time,
    Lag mapdot exp_lag(7.2) ~> Time,
    Fallow convolve Discoveries*Lag,
    Build convolve Fallow*Lag,
    Mature convolve Build*Lag,
    shocks(Shocks),
    (
       Characteristic  = co2 ->
         interpolate(Time, Shocks, Rate),
         Model deplete Mature/Rate,
	 Data tuple Date + Fallow + Build + Mature + Profile + Model,
         Heading = [X,    'Fallow','Built','Reserve', 'Data', 'Model'],
         sumlist(Model, Cumulative_Model)
    ;
       Characteristic = carbon  ->
         Rate mapdot 0.035 ~> Time,
         Model deplete Mature/Rate,
	 Data tuple Date + Profile + Model,
         Heading = [X, 'Data', 'Model'],
         sumlist(Model, Cumulative_Model)
    ;
       Characteristic = both  ->
         discoveries(D),
         Data tuple Date + Discoveries + D,
         Heading = [X, 'Dispersive', 'Discoveries'],
         sumlist(Discoveries, Cumulative_Model)
    ),
    X = 'Date',
    XUnits = ' year',
    YUnits = ' billion barrels/year',
    reply_html_page([title('CO2 rise'),
                     \(con_text:style)],
                    [
		     i('cumulative='), b(Cumulative_Model),
		     \(context_graphing:dygraph_native(lin,
						       Heading,
						       [X,XUnits], ['Production', YUnits],
						       Characteristic, Data))
                    ]
		  ).






