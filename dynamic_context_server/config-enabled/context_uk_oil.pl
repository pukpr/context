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
    [[0,  0.0], %
     [15, 0.0],
     [19, 0.082],
     [24, 0.093],  %
     [27, 0.092],  % Before Piper
     [29, 0.065],  % After Piper
     [33, 0.065], %
     [34, 0.084], %
     [46, 0.2], %
     [50,0.3],
     [300,0.3]]
      ).

shocks1(
    [[0,  0.0], %
     [15, 0.0],
     [19, 0.06],
     [24, 0.065],  %
     [27, 0.06],  % Before Piper
     [29, 0.042],  % After Piper
     [33, 0.042], %
     [34, 0.057], %
     [40, 0.07], %
     [48, 0.055], %
     % [50,0.2],
     [300,0.055]]
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
    shocks1(Shocks),
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
    production3(Profile),
    DD mapdot dd_uk(8, 41, 5.4) ~> Time,
    Discoveries derivative DD/Time,
    Lag mapdot exp_lag(3.2) ~> Time,
    Fallow convolve Discoveries*Lag,
    Build convolve Fallow*Lag,
    Mature convolve Build*Lag,
    shocks1(Shocks),
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
		     i('cumulative='), b('~2f ' - Cumulative_Model),
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
         0,     0,     0,     0,     0,
3.7668E-03,
1.1540E-01,
3.7295E-01,
5.2403E-01,
7.6192E-01,
7.8628E-01,
8.7757E-01,
1.0065E+00,
1.1001E+00,
1.2228E+00,
1.2323E+00,
1.2166E+00,
1.1958E+00,
1.0116E+00,
8.8324E-01,
8.9299E-01,
8.8136E-01,
9.0336E-01,
9.4558E-01,
1.1667E+00,
1.2109E+00,
1.2287E+00,
1.2137E+00,
1.2460E+00,
1.2894E+00,
1.1844E+00,
1.1050E+00,
1.0971E+00,
1.0053E+00,
8.9940E-01,
7.9213E-01,
7.1675E-01,
7.1401E-01,
6.6989E-01,
6.4066E-01,
5.9329E-01,
4.8730E-01,
4.3225E-01,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0]).



production1(
     [	 0,     0,     0,     0,     0,     0,	 0,	0, 0,	0,
         0,     0,     0,     0,     0,
2.8249E-03,
8.6540E-02,
2.7969E-01,
3.9299E-01,
5.7139E-01,
5.8966E-01,
6.5812E-01,
7.5485E-01,
8.2503E-01,
9.1706E-01,
9.2413E-01,
9.1236E-01,
8.9681E-01,
7.5865E-01,
6.6238E-01,
6.6969E-01,
6.6097E-01,
6.7746E-01,
7.0913E-01,
8.7497E-01,
9.0813E-01,
9.2143E-01,
9.1023E-01,
9.3441E-01,
9.6695E-01,
8.8824E-01,
8.2868E-01,
8.2278E-01,
7.5395E-01,
6.7449E-01,
5.9405E-01,
5.3752E-01,
5.3546E-01,
5.0238E-01,
4.8046E-01,
4.4493E-01,
3.6545E-01,
3.2416E-01,
0.0000E+00,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0,
         0,     0,     0,     0,     0,	    0,     0,     0,	 0,	0]).



production3(
     [	 0,     0,     0,     0,     0,
6.30E-04,
5.85E-04,
6.68E-04,
6.28E-04,
6.50E-04,
1.28E-03,
1.78E-03,
2.86E-03,
3.22E-03,
3.56E-03,
1.23E-02,
9.22E-02,
2.89E-01,
4.08E-01,
5.88E-01,
6.07E-01,
6.76E-01,
7.85E-01,
8.77E-01,
9.61E-01,
9.76E-01,
9.75E-01,
9.46E-01,
8.75E-01,
7.04E-01,
7.00E-01,
7.01E-01,
7.23E-01,
7.74E-01,
9.76E-01,
1.00E+00,
9.98E-01,
9.86E-01,
1.02E+00,
1.06E+00,
9.73E-01,
9.04E-01,
8.99E-01,
8.24E-01,
7.40E-01,
6.60E-01,
5.97E-01,
5.98E-01,
5.57E-01,
5.30E-01,
4.89E-01,
4.01E-01]).

% https://www.og.decc.gov.uk/pprs/full_production/annual+oil+production+sorted+by+field+%28tonnes%29/0.htm
% DECC "Petroleum Production Reporting System"
