:- module(context_lightning, [
			      lightningA/5,
			      lightningD/5
			    ]).

:- use_module(context_math).

:- context:register(context_lightning:navigate).
:- context:register(context_lightning:plot).
:- context:register(context_lightning:lightning_indirect_effects_table).

navigate(Request) :-
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Lightning indirect effects'),
	           script([type('text/javascript'),src('/html/js/submit.js')], [])
                   ],
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
				 value('3000')]), i(' <= margin'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Single', 'single'],
                                          ['Multiple', 'multiple']
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('waveform'),
                                 onclick('subm(this.form,"render");')]),
			  input([type('submit'), name(kind), value('table'),
                                 onclick('subm(this.form,"target_iframe");')])
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).


lightningA(I0,Alpha,Beta,X*_Units,Y) :-
   Y is I0*(exp(-Alpha*X)-exp(-Beta*X)).

lightningD(I0,Alpha,Beta,X*_Units,Y) :-
   Y1 is I0*(exp(-Alpha*X)-exp(-Beta*X)),
   Delta = 1e-4,
   (   X > Delta ->
   Y2 is  I0*(exp(-Alpha*(X-Delta))-exp(-Beta*(X-Delta)))
   ;
   Y2 = 0
   ),
   Y is Y1 +Y2.

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
    http_parameters(Request, [kind(waveform, []),
			      limit(Limit, [number]),
                              t_units(TUnits, []),
                              evaluate(Characteristic, [])]),

    Time range [0.0, 0.0002]/0.000001*TUnits,
    (
       Characteristic  = single ->
         rdfS(UID, ent:description, 'Severe stroke'),
         rdfL(UID, ent:i0, I0*Current),
         rdfL(UID, ent:alpha, Alpha/TS ),
         rdfL(UID, ent:beta, Beta/TS),
	 I mapdot lightningA(I0,Alpha,Beta) ~> Time
         % Title = ['Lightning Profile : ', Characteristic]
    ;
       Characteristic = multiple  ->
         rdfS(UID, ent:description, 'Multiple stroke'),
         rdfL(UID, ent:i0, I0*Current),
         rdfL(UID, ent:alpha, Alpha/TS ),
         rdfL(UID, ent:beta, Beta/TS),
	 I mapdot lightningD(I0,Alpha,Beta) ~> Time
         % Title = ['Not Available : ', Characteristic]
    ;
	 I mapdot lightningB(Limit) ~> Time
    ),
    Margin mapdot Limit ~> Time,
    Data tuple Time + Margin + I,
    X = 'time',
    Y = 'current',
    XUnits = TUnits,
    YUnits = Current,
    reply_html_page([title('Lightning Profile'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(lin, [X, 'margin', Y],
						       [X,XUnits], [Y, YUnits],
						       Characteristic, Data))
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


