:- module(context_shock_model, [
				dd/5
			    ]).

/** <module> Shock model
    * Compartmental model
    *
*/

:- use_module(context_math).

:- context:register(context_shock_model:navigate).
:- context:register(context_shock_model:plot).


dd(Exp, C, T, Year, Result) :-
    Result is C/(1+(T/(Year+0.01))^Exp).
/*
shocks(
    [[0, 0.070], % Start of data
     [116, 0.070], % Start of oil embargo
     [117, 0.065], % End of oil embargo
     [122, 0.065], % Start of Iran hostage crisis
     [123, 0.044], % End of recession
     [132, 0.044], % Start of Gulf War
     [134, 0.042], % End of recesssion
     [143, 0.042], % Last good year!
     [145, 0.046], % Running Out??? Why is extraction going up?
     [300, 0.046]]
      ).
*/

shocks(
    [[0,   0.0], % Start of data
     [1,   0.045],
     [79,  0.045],  % Start of depression 1929
     [82,  0.03],  % Full crash  1932
     [90,  0.023], % WWII start
     [92,  0.022], % WWII 1942
     [103,  0.02],  % Post-war recession 1953
     [116, 0.025], % Vietnam war 1966
     [124, 0.033], % Start of oil embargo 1974
     [125, 0.031], % End of oil embargo 1975
     [129, 0.033], % Start of Iran hostage crisis 1979
     [132, 0.025], % End of recession 1982
     [140, 0.024], % Start of Gulf War 1990
     [143, 0.023], % End of recesssion 1993
     [152, 0.025], % Last good year! 2002
     [154, 0.026], % Running Out??? 2004 Why is extraction going up?
     [300, 0.045]]
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
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0
      ]
      ).
/*
production(
     [                 0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,	0,     0,     0, 31803, 34568, 37118, 40436, 43633, 48061,
     50844, 53666, 58463, 58617, 55824, 60412, 62713, 63331, 66049, 62946,
     59533, 57296, 56598, 57683, 57468, 60461, 60785, 63160, 64051, 65470,
     65288, 65788, 66046, 67116, 68103, 69895, 72158, 73586, 72333, 74950,
     74828, 74443, 77054, 80260,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0]).
*/

production(
     [	 0,     0,     0,     0,     0,     0,	 0,	0, 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
0.2,
0.1674,
0.1818,
0.1949,
0.218,
0.2151,
0.2133,
0.2642,
0.2856,
0.2987,
0.3278,
0.3444,
0.3524,
0.3853,
0.4075,
0.432,
0.4575,
0.5029,
0.5035,
0.5559,
0.6889,
0.766,
0.8589,
1.0157,
1.0143,
1.0679,
1.0968,
1.263,
1.325,
1.486,
1.41,
1.373,
1.31,
1.442,
1.522,
1.654,
1.792,
2.039,
1.988,
2.086,
2.15,
2.221,
2.093,
2.257,
2.593,
2.595,
2.745,
3.022,
3.433,
3.404,
3.803,
4.283,
4.519,
4.798,
5.018,
5.626,
6.125,
6.439,
6.608,
7.134,
7.6613,
8.1943,
8.8878,
9.5374,
10.2857,
11.0704,
12.6173,
13.5481,
14.7591,
15.926,
17.5423,
18.5581,
19.5881,
21.3394,
21.3952,
20.3761,
22.05,
22.8899,
23.1154,
24.1075,

21.7385105,	20.45820146,	19.510564,	19.43619562,	19.89211274,	19.69732063,
20.55884838,	20.67727435,	21.42435208,	21.82766868,	22.08131375,	21.94658422,
21.93748769,	21.9631248,	22.32831624,	22.7882914,	23.2936527,	24.01917335,
24.46659495,	24.07809837,	25.01057732,	24.86251254,	24.54996317,	25.31766624,
26.44878788,	26.88006085,	26.74316637,	26.57759995,	26.87433811,	26.36114676,
27.01266834,	27.04024902,
/*
22.9753,
21.7292,
20.913,
20.6572,
21.0543,
20.9762,
22.0694,
22.1865,
23.053,
23.3779,
23.8966,
23.8294,
24.0115,
24.1057,
24.4962,
24.8572,
25.5095,
26.368,
26.8669,
26.4161,
27.3535,
27.2786,
27.1494,
28.1382,
29.2723,
29.5971,
*/

         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0]).

%%   navigate(+Request)
%
%    Dynamic page to shock model
navigate(Request) :-
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Shock model'),
	           script([type('text/javascript'),src('/html/js/submit.js')], [])
                   ],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Plot a shock model'),
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
                                          ['Average', 'average'],
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
    Time range [0, 200]/1,
    shocks(Shocks),
    interpolate(Time, Shocks, Sh),
    Date mapdot 1850 .+ Time,
    Data tuple Date + Sh,

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
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(Characteristic, [default(shocked)])]),

    Time range [0, 300]/1,
    Date mapdot 1850 .+ Time,
    % Margin mapdot Limit ~> Time,
    production(Profile),
    Production shrink Profile/Time,
    % DD mapdot dd(7, 2260, 112) ~> Time,
    DD mapdot dd(7.2, 2260, 114) ~> Time,
    Discoveries derivative DD/Time,
    Lag mapdot exp(7.5) ~> Time,
    Fallow convolve Discoveries*Lag,
    Build convolve Fallow*Lag,
    Mature convolve Build*Lag,
    Reserve shrink Mature/Time,
    shocks(Shocks),
    (
       Characteristic  = shocked ->
         interpolate(Time, Shocks, Rate),
         Model deplete Reserve/Rate,
	 Data tuple Date + Production + Model,
         Heading = [X, 'Data', 'Model']
    ;
       Characteristic = average  ->
         Rate mapdot 0.025 ~> Time,
         Model deplete Reserve/Rate,
	 Data tuple Date + Production + Model,
         Heading = [X, 'Data', 'Model']
    ;
       Characteristic = discovery  ->
         discoveries(D),
	 Disc shrink D/Time,
         Data tuple Date + Discoveries + Disc,
         Heading = [X, 'Dispersive', 'Discoveries']
    ),
    X = 'Date',
    XUnits = ' year',
    YUnits = ' billion barrels/year',
    reply_html_page([title('Shock Profile'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin,
						       Heading,
						       [X,XUnits], ['Production', YUnits],
						       Characteristic, Data))
                    ]
		  ).



/*
    Shocks =
    [[1932.0, 0.070], % Start of data
     [1974.0, 0.070], % Start of oil embargo
     [1974.5, 0.065], % End of oil embargo
     [1980.0, 0.065], % Start of Iran hostage crisis
     [1983.5, 0.044], % End of recession
     [1990.0, 0.044], % Start of Gulf War
     [1992.0, 0.042], % End of recesssion
     [2001.0, 0.042], % Last good year!
     [2003.0, 0.046], % Running Out??? Why is extraction going up?
     [2300.0, 0.046]],
*/
