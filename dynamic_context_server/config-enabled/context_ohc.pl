:- module(context_ohc, [
				dd_ohc/5,
				dd_uniform_ohc/5
			    ]).

/** <module> Ocean Heat Content model
    * Dispersive Diffusion
    *
*/

:- use_module(context_math).

:- context:register(context_ohc:navigate).
:- context:register(context_ohc:plot).


dd_ohc(Scale, L, D, Year, Result) :-
    R is sqrt(D*Year)/L,
    Result is Scale*(R+2)/(R+1)^2/2.

dd_uniform_ohc(Scale, L, D, Year, Result) :-
    (	 Year > 0 ->
    R is L/sqrt(D*Year),
    Result is Scale*(-exp(-R)*(2+R)+2)
    ;
    Result =Scale
    ).

%%   navigate(+Request)
%
%    Dynamic page to OHC model
navigate(Request) :-
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('OHC model'),
	           script([type('text/javascript'),src('/html/js/submit.js')], [])
                   ],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('OHC model'),
                      p('Select a forcing input to model response'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  input([type('text'),
				 name('limit'),
				 value('0')]), i(' <= margin'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Effective', 'response'],
					  ['Linear', 'forcing']
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
                              evaluate(Characteristic, [])]),
    Time range [0, 50]/1,
    (
       Characteristic  = response ->
         forcing(Forcing)
    ;
       Characteristic = forcing  ->
         Scale is 1.55*1.0/50,
         Forcing mapdot Scale .* Time
    ),

    Data tuple Time + Forcing,

    reply_html_page([title('Net Forcing'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin, [date, rate],
						       ['Time', year],
                                                       ['Effective Forcing', 'watts/m^2'],
						       'Net Forcing', Data))
                    ]
		  ).



plot(Request) :-
    http_parameters(Request, [kind(plot, []),
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(Characteristic, [default(shocked)])]),

    TS = 50,
    W2J = 1.126,
    Time range [0, TS]/1,
    D = 0.008,
    Loss = 0.45,
    % Offset = 0.0,
    DD3 mapdot dd_ohc(1.0, 0.3, D) ~> Time,
    DD7 mapdot dd_ohc(1.0, 0.7, D) ~> Time,
    DD100 mapdot dd_ohc(1.0, 100.0, D) ~> Time,
    Scale is  Loss*W2J,
    forcing(F),
    Forcing mapdot Scale .* F,
    Linear_Scale is Loss*W2J*1.55/TS,
    Linear_Forcing mapdot Linear_Scale .* Time,
    (
       Characteristic  = response ->
         OHC3 convolve DD3*Forcing,
         OHC7 convolve DD7*Forcing,
         OHC100 convolve DD100*Forcing,
         % interpolate(Time, Shocks, Rate),
	 Data tuple Time + OHC3 + OHC7 + OHC100,
         Heading = [X,    '0.3km','0.7km', 'All']
    ;
       Characteristic = forcing  ->
         OHC3 convolve DD3*Linear_Forcing,
         OHC7 convolve DD7*Linear_Forcing,
         OHC100 convolve DD100*Linear_Forcing,
	 Data tuple Time + OHC3 + OHC7 + OHC100,
         Heading = [X,    '0.3km','0.7km', 'All']
    ),
    X = 'Time',
    XUnits = ' year',
    YUnits = ' 10^22 joules',
    reply_html_page([title('OHC'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin,
						       Heading,
						       [X,XUnits],
                                                       ['Heat', YUnits],
						       Characteristic, Data))
                    ]
		  ).

forcing1([
0.4,
0.3,
0.2,
-0.2,
-1,
-1,
-0.2,
0.2,
0.1,
-0.1,
0.2,
0.5,
0.65,
0.6,
0.5,
0.15,
0.3,
0.7,
0.8,
0.85,
0.95,
1.05,
0.2,
-0.4,
0,
0.7,
0.9,
1.02,
1.2,
1.4,
1.4,
0.5,
-1.0,
-0.4,
0.8,
1.2,
1.32,
1.4,
1.5,
1.6,
1.65,
1.63,
1.61,
1.58,
1.54,
1.49,
1.51,
1.55,
1.58,
1.59,
1.61]).

forcing([
0.398,
0.206,
0.137,
-0.621,
-1.215,
-0.516,
0.015,
0.274,
0.007,
-0.11,
0.343,
0.557,
0.654,
0.567,
0.466,
0.061,
0.493,
0.765,
0.813,
0.862,
1.008,
1.056,
-0.088,
-0.599,
0.452,
0.882,
0.902,
1.047,
1.214,
1.379,
1.4,
0.278,
-1.316,
0.341,
0.998,
1.245,
1.322,
1.364,
1.479,
1.555,
1.594,
1.585,
1.589,
1.588,
1.552,
1.515,
1.514,
1.504,
1.53,
1.518,
1.6,
1.635]).















