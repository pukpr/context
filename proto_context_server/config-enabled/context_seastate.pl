:- module(context_seastate, [
			      waveHeight/5
			    ]).

:- use_module(context_math).

:- context:register(context_seastate:navigate).
:- context:register(context_seastate:plot).
:- context:register(context_seastate:seastate_table).

navigate(_Request) :-
   collect_unit_options(ent:length, Lunits),

   reply_html_page(cliopatria(default),
                   [title('Seastate Probabilities')],
                   [\(con_text:table_with_iframe_target(
                                    target_iframe,
		     [
                      h1('Seastate as Significant Wave Height'),
                      p('Likelihood of specific sea state depends on region'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('l_units')], Lunits),
			  input([type('text'),
				 name('limit'),
				 value('3')]), i(' <= seastate'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Atlantic coast', 'atlantic'],
                                          ['Lake Superior', 'superior'],
                                          ['Lake Michigan', 'michigan']
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('lin')]),
			  input([type('submit'), name(kind), value('log')])
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).

check_margin(Z, Limit, 'Margin failed') :-
    last(Z, Last),
    Last < Limit,
check_margin(_Z, _Limit, 'Margin OK').

find_cumulative_point(Variant, [F*m|_], Input, Output) :-
    F > Variant,
    !, reverse(Input, Output).
find_cumulative_point(Variant, [F*m|R], Input, Output) :-
    % atom_number(F, N),
    !, find_cumulative_point(Variant, R, [F|Input], Output).

% member(P, List), P<Probability.

plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [atom]),
                              l_units(LUnits, []),
                              evaluate(Characteristic, [])]),

    sea_state_high(Limit, High*LUnits),
    sea_state_low(Limit, Low*LUnits),
    context_units:convert(0.01*m, H1*LUnits, H1),
    context_units:convert(20.0*m, H2*TUnits, H2),

    H range [H1, H2]^0.9*LUnits,
    (
       Characteristic = atlantic ->
	 Z mapdot waveHeight(cdf, 2.0, 1.0) ~> H
     ;
       Characteristic = superior ->
	 Z mapdot waveHeight(cdf, 1.0, 1.0) ~> H
      ;
       Characteristic = michigan ->
	 Z mapdot waveHeight(cdf, 0.5, 1.0) ~> H
    ),
    %  switch this to get info
    find_cumulative_point(Low, H, [], H_low),
    find_cumulative_point(High, H, [], H_high),
    % Margins1 mapdot 0.0 ~> Z,
    % Margins2 mapdot 0.1 ~> Z,

    Z1 shrink Z/H_low,
    last(Z1, P_low),
    Z_low mapdot P_low ~> H,

    Z2 shrink Z/H_high,
    last(Z2, P_high),
    Z_high mapdot P_high ~> H,

    Data tuple H + Z + Z_low + Z_high,
    X = 'Wave Height',
    Y = 'Cumulative Probability',
    YL = 'Sea State Limit Low',
    YH = 'Sea State Limit High',
    XUnits = TUnits,
    YUnits = LUnits,
    reply_html_page([title('Sea State at Wave Height'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(Kind, [X, Y, YL, YH],
						       [X,XUnits], [Y, YUnits],
						       ['Sea State at Wave Height', Low, High], Data))
                    ]
		  ).


waveHeight(Mean, Depth,  H*Height, P*cdf) :-
   context_units:convert(H*Height, H1*m, H1),
   P is exp(-H1/Mean*Depth/Depth),
   !.
waveHeight(cdf, Mean, Depth,  H*Height, P) :-
   waveHeight(Mean, Depth,  H*Height, P*cdf).


sea_states([Value, Low, High, Units, Description]) :-
    rdfS(UID, ent:sea_state, Value),
    rdfL(UID, ent:wave_height, [Low,High]),
    rdfS(UID, ent:description, Description),
    rdfS(UID, ent:unit, Units).

sea_state_low(Value, Low*Units) :-
    rdfS(UID, ent:sea_state, Value),
    rdfL(UID, ent:wave_height, [Low,_]),
    rdfS(UID, ent:unit, Units).
sea_state_high(Value, High*Units) :-
    rdfS(UID, ent:sea_state, Value),
    rdfL(UID, ent:wave_height, [_,High]),
    rdfS(UID, ent:unit, Units).


seastate_table(_Request) :-
    findall(Row,sea_states(Row), Rows),
    reply_html_page([title('Sea State Table'),
		     \(con_text:style)],
                   [
		    h1('Sea State Ranges'),
		    p('The probability of a specific sea state depends on the characteristics of the environment'),
		    \(con_text:table_multiple_entries(
				   [[b(value), b(low), b(high), b(units), b(description)]],
				    Rows
						     )
		     )
                   ]).

