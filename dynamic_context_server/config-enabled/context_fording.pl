:- module(context_fording, [
			      speed_scaling/5,
                              power_raise/2
			    ]).

:- use_module(context_math).

:- context:register(context_fording:navigate).
:- context:register(context_fording:plot).

% http://waterdata.usgs.gov/nwis/dv?referred_module=sw&site_no=01100000
collect_names_options(List) :-
    findall(option([value(ID)],[Name]),
            (   rdf(ID, ent:river, _Data),
	        rdf(ID, ent:title, Name)
            ),
            List).

navigate(Request) :-
   collect_unit_options(ent:area, Lunits),
   collect_names_options(Rivers),

   reply_html_page(cliopatria(default),
                   [title('Fording models')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Fording model'),
                      p(['A body of water has a statistical size and flow. ',
'Lakes have an areal coverage while rivers have a width, depth, and flow rate. ',
'Rivers also show seasonal variation in flow rate modeled as a stochastic process']),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('a_units')], Lunits),
			  input([type('text'),
				 name('cross_section'),
				 value('3')]), i(' <= cross section'),
			  br([]),
			  p(['select river',
			  select([name('rivers')], Rivers)
			    ]),
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


speed_scaling(AreaUnits, Factor, Cross, X, Y) :-
   XF is X * Factor,
   Value is 10^XF,
   context_units:convert(Cross*AreaUnits, Scaled*ft^2, Scaled),
   Speed is Value/Scaled,
   Y is log(Speed)/log(10).

power_raise(X, Y) :-
    Y is 10^X.

log_factor(0.1).



plot(Request) :-
    http_parameters(Request, [kind(log, []),
			      cross_section(CrossSection, [number]),
                              a_units(AUnits, []),
                              rivers(River, [])]),

    rdfL(River, ent:river, Flow_Data), % ft^3/s
    get_location(River, Lat, Lon, Title),
    atom_to_term(AUnits, A, []),
    log_factor(Fraction),
    length(Flow_Data, L),
    Last is (L - 1),
    Axis range [0, Last]/1,
    Flow mapdot speed_scaling(A, Fraction, CrossSection) ~> Axis,
    Data tuple Flow + Flow_Data,
    X = 'log current velocity',
    Y = 'count',
    XUnits = 'ft/sec',
    YUnits = '',
    reply_html_page([title('River Flow'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(log, [X, Y],
						       [X,XUnits], [Y, YUnits],
						       'Margin for flow', Data)),
		     \(con_text:button_link('Display Map',
					   '/context_map/navigate',
					   render,
					   [[lat, Lat],
					    [lon, Lon],
					    [title, Title]
					   ]))

                    ]
		  ).

plot(Request) :-
    http_parameters(Request, [kind(lin, []),
			      cross_section(CrossSection, [number]),
                              a_units(AUnits, []),
                              rivers(River, [])]),

    rdfL(River, ent:river, Flow_Data), % ft^3/s
    get_location(River, Lat, Lon, Title),
    atom_to_term(AUnits, A, []),
    log_factor(Fraction),
    length(Flow_Data, L),
    Last is (L - 1),
    Axis range [0, Last]/1,
    non_zero(Axis, Flow_Data, [], [], AX, FD),
    Flow_Prob normalize FD,

    % Linearized_Axis range [1, 10000000000]^1.26,
    Flow mapdot speed_scaling(A, Fraction, CrossSection) ~> AX,
    Linear_Flow mapdot power_raise ~> Flow,
    Data tuple Linear_Flow + Flow_Prob,
    X = 'current velocity',
    Y = 'probability',
    XUnits = 'ft/sec',
    YUnits = 'density',
    reply_html_page([title('River Flow'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(log, [X, Y],
						       [X,XUnits], [Y, YUnits],
						       'PDF Margin for flow', Data)),
		     \(con_text:button_link('Display Map',
					   '/context_map/navigate',
					   render,
					   [[lat, Lat],
					    [lon, Lon],
					    [title, Title]
					   ]))
                    ]
		  ).



non_zero([], [], Range, Data, RFinal, DFinal) :-
    reverse(Range, RFinal),
    reverse(Data, DFinal), !.
non_zero([FR|RR], [FD|RD], Range, Data, RFinal, DFinal) :-
    FD > 0.0,
    !, non_zero(RR, RD, [FR|Range], [FD|Data], RFinal, DFinal).
non_zero([_|RR], [_|RD], Range, Data, RFinal, DFinal) :-
    !, non_zero(RR, RD, Range, Data, RFinal, DFinal).









