:- module(context_shock_model, [
			    ]).

/** <module> Shock model
    * Compartmental model
    *
*/

:- use_module(context_math).

:- context:register(context_shock_model:navigate).
:- context:register(context_shock_model:plot).

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
					 [['Production', 'production'],
                                          ['Cumulative', 'cumulative']
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

%%   extract(+Cumulative, +Avail, +Extract, +Prod, -Production)
%
%    Extraction routine
extraction(_, [],[], P, Production) :- reverse(P,Production).
extraction(Cumulative, [A|Avail], [R|Extract], Prod, Production):-
   C is Cumulative + A - R*Cumulative,
   P is R*C,
   extraction(C, Avail, Extract, [P|Prod], Production).
% C := C + Discovery_Window (Time, Start) * DT - C * Rate * DT;
% P := Rate * C;

search_interpolate(N, [[N1,V1],[N2,V2]], Val) :-
    Val is V1 + (N-N1)*(V2-V1)/(N2-N1).
search_interpolate(N, [[N1,V1],[N2,V2]|_], Val) :-
    N >= N1, N < N2,
    Val is V1 + (N-N1)*(V2-V1)/(N2-N1).
search_interpolate(N, [[_,_],[N2,V2]|Table], Val) :-
    search_interpolate(N, [[N2,V2]|Table], Val).

interpolate([], _, Vals, Values) :- reverse(Vals, Values), !.
interpolate([N|Nums], Table, Vals, Values):-
    search_interpolate(N, Table, Val),
    !, interpolate(Nums, Table, [Val|Vals], Values).

%%   plot(+Request)
%
%    Plot the lightning stroke temporal profile
plot(Request) :-
    http_parameters(Request, [kind(table, []),
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(_Table, [])]),
    findall(Row,effects_table(Row), Rows),
    reply_html_page([title('Lightning Indirect Effects Table'),
		     \(con_text:style)],
                   [
		    h1('Lightning indirect effects waveform parameters'),
		    p('A lightning stroke waveform is parameterized according to profile'),
		    \(con_text:table_multiple_entries(
				   [[b('Current component'), b('Description'), b('I0'), b('alpha'),
				     b('beta'), b('Peak current'), b('Action integral'), b('Decay to 50%'),
				     b('Time to 10%'), b('Time to 90%'), b('Time to Peak'),
				     b('Rate of rise'), b('Peak rate of rise')]],
				    Rows
						     )
		     )
                   ]).



plot(Request) :-
    http_parameters(Request, [kind(plot, []),
			      limit(Limit, [number]),
                              t_units(TUnits, []),
                              evaluate(Characteristic, [default(production)])]),

    Time range [0, 150]/1,
    Discoveries =
     [             9.0,  4.0,  4.5,  3.0,  5.0,  5.5, 42.0, 42.5, % 1930's
      52.0, 18.0, 16.0,  5.0,  3.0,  7.4,  7.6,  7.0, 49.0, 53.0, % 1940's
      54.0, 19.5, 16.0, 26.0, 20.0, 28.0, 24.0, 35.0, 40.0, 40.5, % 1950's
      42.0, 44.0, 50.0, 41.0, 50.0, 48.5, 47.5, 32.0, 30.5, 30.4, % 1960's
      29.5, 40.5, 37.0, 38.5, 24.0, 26.5, 31.0, 37.0, 38.0, 36.0, % 1970's
      26.0, 24.0, 20.0, 20.0, 22.0, 21.0, 20.5, 18.0, 16.2, 17.5, % 1980's
      16.5, 20.0, 17.0, 16.0,  9.0,  8.5,  9.0,  9.0,  9.5, 14.0, % 1990's
      18.0, 17.5, 13.0, 9.0,   9.0], % 2004
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
    Shocks =
    [[0, 0.070], % Start of data
     [42, 0.070], % Start of oil embargo
     [43, 0.065], % End of oil embargo
     [48, 0.065], % Start of Iran hostage crisis
     [49, 0.044], % End of recession
     [58, 0.044], % Start of Gulf War
     [60, 0.042], % End of recesssion
     [69, 0.042], % Last good year!
     [71, 0.046], % Running Out??? Why is extraction going up?
     [300, 0.046]],

    Lag mapdot exp(8) ~> Time,
    Fallow convolve Discoveries*Lag,
    Build convolve Fallow*Lag,
    Mature convolve Build*Lag,
    (
       Characteristic  = production ->
         M shrink Mature/Time,
         % Sh mapdot (-1932) .+
         interpolate(Time, Shocks, [], Sh),
	 extraction(0.0, M, Sh, [], Extract),
         true          % Title = ['Lightning Profile : ', Characteristic]
    ;
       Characteristic = cumulative  ->
         Rate mapdot exp(20) ~> Time, % Conventional
         Extract convolve Mature*Rate,
         true         % Title = ['Not Available : ', Characteristic]
    ;
         Flat mapdot 0.05 ~> Time,
         M shrink Mature/Time,
	 extraction(0.0, M, Flat, [], Extract),
         true
    ),
    Margin mapdot Limit ~> Time,
    P shrink Extract/Time,
    Date mapdot 1932 .+ Time,
    Data tuple Date + Margin + P,
    X = 'date',
    Y = 'production',
    XUnits = 'year',
    YUnits = 'barrels',
    reply_html_page([title('Shock Profile'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin, [X, 'margin', Y],
						       [X,XUnits], [Y, YUnits],
						       Characteristic, Data))
                    ]
		  ).


