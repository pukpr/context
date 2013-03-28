:- module(context_uk_oil, [
				dd_uk/5
				% exp_lag/3
			    ]).

/** <module> UK North Sea Shock model
    * Compartmental model
    *
*/

:- use_module(context_math).

:- context:register(context_uk_oil:navigate).
:- context:register(context_uk_oil:plot).

/*
exp_lag(Mean, X, Y) :-
    (	X = 0 ->
        Y = 1.0/Mean * (exp(-X/Mean)+exp(-(X+1.0)/Mean))/2
    ;
        Y = 1.0/Mean * (exp(-X/Mean)+exp(-(X+1.0)/Mean))/2

    ).
*/

dd_uk(Delay, C, T, Year, Result) :-
    (   Year > Delay ->
        Result is C*exp(-T/(Year-Delay+0.01))
    ;
        Result = 0.0
    ).
    % Result is C/(1+(T/(Year+0.01))^Exp).


shocks(
    [[0,   0.0], % Start of data
     [1,   0.156],
     [9,  0.156],  % Start of depression 1929
     [45, 0.131],  % Post-war recession 1955
     [114, 0.033], % Vietnam war 1964
     [123, 0.048], % Start of oil embargo 1973
     [124, 0.046], % Start of oil embargo 1974
     [125, 0.044], % End of oil embargo 1975
     [129, 0.046], % Start of Iran hostage crisis 1979
     [132, 0.035], % End of recession 1982
     [140, 0.034], % Start of Gulf War 1990
     [143, 0.0325], % End of recesssion 1993
     [152, 0.036], % Last good year! 2002
     [154, 0.038], % Running Out??? 2004 Why is extraction going up?
     [300, 0.125]]
      ).


%%   navigate(+Request)
%
%    Dynamic page to shock model
navigate(Request) :-
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Shock model - UK North Sea'),
	           script([type('text/javascript'),src('/html/js/submit.js')], [])
                   ],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Plot a shock model - UK North Sea'),
                      p('Stages fallow to extraction describe the profile'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  input([type('text'),
				 name('limit'),
				 value('0')]), i(' <= margin'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Shocked', 'shocked'],
                                          % ['Average', 'average'],
					  ['Discovery', 'discovery']
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
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(_Table, [])]),
    Time range [0, 80]/1,
    shocks(Shocks),
    interpolate(Time, Shocks, Sh),
    Date mapdot 1960 .+ Time,
    Data tuple Date + Sh,

    reply_html_page([title('Shock Extraction'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin, [date, rate],
						       ['Date', year],
                                                       ['Rate', fraction],
						       'extraction rate', Data))
                    ]
		  ).



plot(Request) :-
    http_parameters(Request, [kind(plot, []),
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(Characteristic, [default(shocked)])]),

    Time range [0, 80]/1,
    Date mapdot 1960 .+ Time,
    production(Profile),
    DD mapdot dd_uk(8, 41, 5.4) ~> Time,
    Discoveries derivative DD/Time,
    Lag mapdot exp_lag(3.2) ~> Time,
    Fallow convolve Discoveries*Lag,
    Build convolve Fallow*Lag,
    Mature convolve Build*Lag,
    shocks(Shocks),
    (
       Characteristic  = shocked ->
         interpolate(Time, Shocks, Rate),
         Model deplete Mature/Rate,
	 Data tuple Date + Fallow + Build + Mature + Profile + Model,
         Heading = [X,    'Fallow','Built','Reserve', 'Data', 'Model'],
         sumlist(Model, Cumulative_Model)
    ;
       Characteristic = discovery  ->
         discoveries(D),
         Data tuple Date + Discoveries + D,
         Heading = [X, 'Dispersive', 'Discoveries'],
         sumlist(Discoveries, Cumulative_Model)
    ),
    X = 'Date',
    XUnits = ' year',
    YUnits = ' billion barrels/year',
    reply_html_page([title('Shock Profile'),
                     \(con_text:style)],
                    [
		     i('cumulative='), b(Cumulative_Model),
		     \(context_graphing:dygraph_native(lin,
						       Heading,
						       [X,XUnits],
                                                       ['Production', YUnits],
						       Characteristic, Data))
                    ]
		  ).




discoveries(
     [   0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,    0,  9.0,  4.0,  4.5,  3.0,  5.0,  5.5, 42.0, 42.5, % 1930's
      52.0, 18.0, 16.0,  5.0,  3.0,  7.4,  7.6,  7.0, 49.0, 53.0, % 1940's
      54.0, 19.5, 16.0, 26.0, 20.0, 28.0, 24.0, 35.0, 40.0, 40.5, % 1950's
      42.0, 44.0, 50.0, 41.0, 50.0, 48.5, 47.5, 32.0, 30.5, 30.4, % 1960's
      29.5, 40.5, 37.0, 38.5, 24.0, 26.5, 31.0, 37.0, 38.0, 36.0, % 1970's
      26.0, 24.0, 20.0, 20.0, 22.0, 21.0, 20.5, 18.0, 16.2, 17.5, % 1980's
      16.5, 20.0, 17.0, 16.0,  9.0,  8.5,  9.0,  9.0,  9.5, 14.0, % 1990's
      18.0, 17.5, 13.0, 9.0,   9.0,  0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0
      ]
      ).


production(
     [	 0,     0,     0,     0,     0,     0,	 0,	0, 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
       0.2,0.1674,0.1818,0.1949,0.218,0.2151,0.2133,0.2642,0.2856, 0.2987,
       0.3278,0.3444,0.3524,0.3853,0.4075,0.432,0.4575,0.5029,0.5035,0.5559,
       0.6889,0.766,0.8589,1.0157,1.0143,1.0679,1.0968,1.263,1.325,1.486,
       1.41,1.373,1.31,1.442,1.522,1.654,1.792,2.039,1.988,2.086,
       2.15,2.221,2.093,2.257,2.593,2.595,2.745,3.022,3.433,3.404,
       3.803,4.283,4.519,4.798,5.018,5.626,6.125,6.439,6.608,7.134,
       7.6613,8.1943,8.8878,9.5374,10.2857,11.0704,12.6173,13.5481,14.7591,15.926,
       17.5423,18.5581,19.5881,21.3394,21.3952,20.3761,22.05,22.8899,23.1154,24.1075,
       21.7385105,20.45820146,19.510564,19.43619562,	19.89211274,	19.69732063,       20.55884838,	20.67727435,	21.42435208,
       21.82766868,22.08131375,21.94658422,21.93748769,	21.9631248,	22.32831624,	22.7882914,	23.2936527,	24.01917335,
       24.46659495,24.07809837,25.01057732,24.86251254,	24.54996317,	25.31766624,       26.44878788,	26.88006085,	26.74316637,	26.57759995,	26.87433811,	26.36114676,       27.01266834,	27.04024902,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0]).
