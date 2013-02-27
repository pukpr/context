:- module(context_shock_model, [
				dd/5,
				exp_lag/3
			    ]).

/** <module> Shock model
    * Compartmental model
    *
*/

:- use_module(context_math).

:- context:register(context_shock_model:navigate).
:- context:register(context_shock_model:plot).


exp_lag(Mean, X, Y) :-
    (	X = 0 ->
        Y = 1.0/Mean * (exp(-X/Mean)+exp(-(X+1.0)/Mean))/2
    ;
        Y = 1.0/Mean * (exp(-X/Mean)+exp(-(X+1.0)/Mean))/2

    ).


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
/*
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
*/
shocks(
    [[0,   0.0], % Start of data
     [1,   0.056],
     [79,  0.056],  % Start of depression 1929
%     [82,  0.03],  % Full crash  1932
%     [90,  0.023], % WWII start
%     [92,  0.022], % WWII 1942
     [105, 0.031],  % Post-war recession 1955
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


shocks_plateau(
    [[0,   0.0], % Start of data
     [1,   0.056],
     [79,  0.056],  % Start of depression 1929
     [105, 0.031],  % Post-war recession 1955
     [114, 0.033], % Vietnam war 1964
     [123, 0.048], % Start of oil embargo 1973
     [124, 0.046], % Start of oil embargo 1974
     [125, 0.045], % End of oil embargo 1975
     [129, 0.047], % Start of Iran hostage crisis 1979
     [132, 0.044], % End of recession 1982
     [140, 0.041], % Start of Gulf War 1990
     [143, 0.040], % End of recesssion 1993
     [152, 0.040], % Last good year! 2002
     [154, 0.040], % Running Out??? 2004 Why is extraction going up?
     [300, 0.125]]
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

discoveries_lahererre(
 [   0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
     0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
     0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
     0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
     0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
  7.267266393, 1.292900044, 0.14028755, 1.257656536, 2.173433499, 1.24202653, 0.444594965, 1.525446972, 2.099670307, 2.473460725,
  2.896213462, 1.904731961, 1.902003548, 2.140674221, 2.012293036, 3.032034124, 2.051491795, 1.231931058, 2.927962735, 3.07739287,
  2.247662534, 2.754136369, 2.839154489, 2.626142989, 3.098471341, 12.17054031, 20.95590692, 41.41498944, 6.299512511, 19.31533176,
  2.831161565, 5.418768015, 2.797014851, 3.213937483, 3.982977375, 22.29002013, 10.86768205, 69.64410665, 5.433476149, 36.12333342,
  12.53381773, 4.052871007, 2.68457478, 9.017744691, 15.86046006, 4.363224471, 4.005085939, 140.0257019, 20.15522157, 11.25929264,
  41.3904985, 6.275633125, 40.75112114, 21.81029243, 28.45388702, 26.06833557, 53.9054679, 51.31971036, 27.85804446, 59.69461177,
  48.46395283, 50.42398757, 28.87561726, 80.54130803, 45.30477689, 26.50368223, 27.99987946, 51.17878304, 28.71414193, 20.4481972,
  74.48612417, 14.58134882, 28.26515128, 30.54384695, 25.45070934, 46.07166744, 48.26350897, 21.78302561, 40.38297124, 14.91281457,
  16.29003059, 19.15406212, 12.8178232, 19.34611373, 19.10874979, 13.58997488, 12.31575757, 15.05768854, 14.18651602, 11.92003056,
  8.782264962, 7.508174033, 6.560278105, 7.830966277, 7.501170463, 11.28681879, 7.370509, 10.819441, 18.060482, 16.246747,
  11.774413,   8.721368,    8.077012,
     8.9,     8.5,	    8.0,     7.7,     7.3,	 6.9,	6.6,
         6.3,     6.0,     5.7,     5.4,     0,	    0,     0,     0,	 0,	0,
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
/* 0ld
22.9753,21.7292,20.913,20.6572,21.0543,20.9762,22.0694,22.1865,23.053,23.3779,
23.8966,23.8294,24.0115,24.1057,24.4962,24.8572,25.5095,26.368,26.8669,26.4161,
27.3535,27.2786,27.1494,28.1382,29.2723,29.5971,
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
    DD mapdot dd(8, 2200, 114) ~> Time,
    Discoveries derivative DD/Time,
    %discoveries_lahererre(DD),
    %Discoveries shrink DD/Time,
    Lag mapdot exp_lag(7.2) ~> Time,
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
       Characteristic = average  ->
         Rate mapdot 0.035 ~> Time,
         Model deplete Mature/Rate,
	 Data tuple Date + Profile + Model,
         Heading = [X, 'Data', 'Model'],
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
