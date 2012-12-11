:- module(context_seastate, [
			      waveHeight/5
			    ]).

:- use_module(context_math).

:- context:register(context_seastate:navigate).
:- context:register(context_seastate:plot).
:- context:register(context_seastate:seastate_table).

navigate(Request) :-
   collect_unit_options(ent:length, Lunits),
   SS = a([href(seastate_table), target(render)], 'sea-state'),

   reply_html_page(cliopatria(default),
                   [title('Seastate Probabilities')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Seastate as Significant Wave Height'),
                      p('Likelihood of specific sea state depends on region'),
                      form([action(plot), target(target_iframe)],
			 [
			  \(con_text:number_radio_toggles('limit',
							  [1,2,3,4,5,6,7,8,9])),
			  i([' <= ',  SS]),
/*
			  input([type('text'),
				 name('limit'),
				 value('3'),
				 size(5)]),
*/
			  br([]),
			  select([name('l_units')], Lunits),
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
    Last =< Limit,
check_margin(_Z, _Limit, 'Margin OK').

find_cumulative_point(0.0, _, _, [0,1]).

find_cumulative_point(Variant, [F*_|_], Input, Output) :-
    F > Variant, % + 0.01,
    !, reverse(Input, Output).
find_cumulative_point(Variant, [F*_|R], Input, Output) :-
    % atom_number(F, N),
    !, find_cumulative_point(Variant, R, [F|Input], Output).
find_cumulative_point(_, _, Input, Output) :-
    !, reverse(Input, Output).

% member(P, List), P<Probability.

plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [atom]),
                              l_units(LUnits, [atom]),
                              evaluate(Characteristic, [default(atlantic)])]),

    sea_state_high(Limit, High*m),
    sea_state_low(Limit, Low*m),
    % Low is L + 0.000001,
    Smallest = 0.001,  % 0.001
    (
       Characteristic = atlantic ->
          Highest  = 20.0,   % 20.0
          HM range [Smallest, Highest]^0.95,
	 % Z mapdot waveHeight(cdf, 2.0, 1.0) ~> H
          r_open_session,
           Z mapdot bessel_seastate(1,0.81,26.0,cdf) ~> HM,
          r_close
	   % Z mapdot Scale .* ZS
     ;
       Characteristic = superior ->
          Highest  = 15.0,   % 20.0
          HM range [Smallest, Highest]^0.95,
          r_open_session,
	    % Z mapdot waveHeight(cdf, 1.0, 1.0) ~> H
            Z mapdot bessel_seastate(0,0.85,16.0,cdf) ~> HM,
           r_close
      ;
       Characteristic = michigan ->
          Highest  = 10.0,   % 20.0
          HM range [Smallest, Highest]^0.95,
	 % Z mapdot waveHeight(cdf, 0.5, 1.0) ~> H
          r_open_session,
           Z mapdot bessel_seastate(0,0.56,12.5,cdf) ~> HM,
          r_close
    ),
    context_units:convert(1*m, Scale*LUnits, Scale),
    context_units:convert(Smallest*m, H1*LUnits, H1),
    context_units:convert( Highest*m, H2*LUnits, H2),
    context_units:convert(Low*m, H1*LUnits, Lo),
    context_units:convert(High*m, H2*LUnits, Hi),

    H range [H1, H2]^0.95*LUnits,

    %  switch this to get info
    find_cumulative_point(Lo, H, [], H_low),
    find_cumulative_point(Hi, H, [], H_high),
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
    XUnits = LUnits,
    YUnits = 'fraction',
    reply_html_page([title('Sea State at Wave Height'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(Kind, [X, Y, YL, YH],
						       [X,XUnits], [Y, YUnits],
						       ['Sea State ', Limit,
							' from ', Lo, ' .. ', Hi,
							' ', XUnits], Data))
                    ]
		  ).


waveHeight(Mean, Depth,  H*Height, P*cdf) :-
   context_units:convert(H*Height, H1*m, H1), % Fix this for actual wave height
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
    % Low is L + 0.0001,
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

