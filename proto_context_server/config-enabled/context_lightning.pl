:- module(context_lightning, [
			      lightningA/3,
			      lightningB/3,
			      lightningC/3
			    ]).

:- use_module(context_math).

:- context:register(context_lightning:navigate).
:- context:register(context_lightning:plot).
:- context:register(context_lightning:lightning_indirect_effects_table).

navigate(Request) :-
   collect_unit_options(ent:time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Lightning indirect effects')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Plot a lightning current waveform'),
                      p('Stages A through H describe the waveform'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  input([type('text'),
				 name('limit'),
				 value('3')]), i(' <= margin'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Single', 'single'],
                                          ['Multiple', 'multiple'],
					  [ 'Table', 'table']
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('lin')])
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).


lightningA(Parameter,X*_Units,Y) :-
   Y is sin(Parameter*X).
lightningB(Parameter,X*_Units,Y) :-
   exp(Parameter, X, Y).
lightningC(Parameter,X*_Units,Y) :-
   Y is Parameter*X.

plot(Request) :-
    http_parameters(Request, [kind(_Kind, []),
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(table, [])]),
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
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [number]),
                              t_units(TUnits, []),
                              evaluate(Characteristic, [])]),

    Time range [0.0, 10.0]/1.0*TUnits,
    (
       Characteristic  = single ->
	 I mapdot lightningA(Limit) ~> Time
     ;
       Characteristic = multiple  ->
	 I mapdot lightningB(Limit) ~> Time
      ;
	 I mapdot lightningB(Limit) ~> Time
    ),
    Data tuple Time + I,
    X = 'xname',
    Y = 'yname',
    XUnits = 'units',
    YUnits = TUnits,
    reply_html_page([title('Lightning Profile'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(Kind, [X, Y],
						       [X,XUnits], [Y, YUnits],
						       'Lightning Profile', Data))
                    ]
		  ).


effects_table([CC, Desc, I0, Alpha, Beta, PeakI, ActionInt, Decay50, Time10, Time90, TimePeak, RR, PRR]) :-
    rdfS(UID, ent:current_component, CC),
    rdfS(UID, ent:description, Desc),
    rdfS(UID, ent:i0, I0),
    rdfS(UID, ent:alpha, Alpha ),
    rdfS(UID, ent:beta, Beta),
    rdfS(UID, ent:peak_current, PeakI),
    rdfS(UID, ent:action_integral, ActionInt),
    rdfS(UID, ent:decay50percent, Decay50),
    rdfS(UID, ent:time10percent, Time10),
    rdfS(UID, ent:time90percent, Time90),
    rdfS(UID, ent:time_to_peak, TimePeak),
    rdfS(UID, ent:rate_of_rise, RR),
    rdfS(UID, ent:peak_rate_of_rise, PRR).


