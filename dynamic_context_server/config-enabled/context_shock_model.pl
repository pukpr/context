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

    Time range [0, 250]/1,
    Discoveries = [1,0],
    Decline mapdot exp(26) ~> Time,
    Fallow convolve Discoveries*Decline,
    Build convolve Decline*Fallow,
    Mature convolve Decline*Build,
    Extract convolve Mature*Decline,

    (
       Characteristic  = production ->
         true
         % Title = ['Lightning Profile : ', Characteristic]
    ;
       Characteristic = cumulative  ->
         true
         % Title = ['Not Available : ', Characteristic]
    ;
         true
    ),
    Margin mapdot Limit ~> Time,
    P shrink Extract/Time,
    Data tuple Time + Margin + P,
    X = 'time',
    Y = 'current',
    XUnits = 'years',
    YUnits = 'barrels',
    reply_html_page([title('Shock Profile'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin, [X, 'margin', Y],
						       [X,XUnits], [Y, YUnits],
						       Characteristic, Data))
                    ]
		  ).


