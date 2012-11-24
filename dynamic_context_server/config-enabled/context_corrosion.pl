:- module(context_corrosion, [
			      corrosionModel/6
			     ]).

:- use_module(context_math).

:- context:register(context_corrosion:navigate).
:- context:register(context_corrosion:plot).
:- context:register(context_corrosion:corrosion_scale_table).

% rdf_(ent:steel, ent:diffusion, ent:steel).

/*
collect_unit_options(Functor, List) :-
    findall(option([value(Value)],[Name]),
            (   rdf_(Functor, ent:units, Unit),
                rdf_(Unit, ent:unit, Value),
                rdf_(Unit, ent:description, Name)
            ),
            List).
*/




navigate(Request) :-
   collect_unit_options(ent:length, Lunits),
   collect_unit_options(ent:time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Corrosive Atmosphere')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Corrosive Atmosphere Models'),
                      p('Mild steel in selected environments'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  select([name('l_units')], Lunits),
			  input([type('text'),
				 name('limit'),
				 value('40')]), i(' <= margin'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Dry rural area', 'rural'],
                                          ['Marine (severe)', 'marine_severe'],
                                          ['Marine (mild)', 'marine_mild'],
                                          ['Industrial area', 'industrial'],
                                          ['Urban area', 'urban']
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

get_location(Characteristic, Lat, Lon, Location_Name, Class, EC, Examples) :-
    rdfS(U, ent:corrosive_atmosphere, Characteristic),
    rdf(U, ent:locale, Loc),
    rdf(U, ent:corrosion_class, CC),
    rdfS(CC, ent:corrosion_scale, Class),
    rdfS(CC, ent:environmental_conditions, EC),
    rdfS(CC, ent:exterior_examples, Examples),
    rdfR(Loc, ent:lat, Lat),
    rdfR(Loc, ent:lon, Lon),
    rdfS(Loc, ent:title, Location_Name).

plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(Limit, [number]),
                              t_units(TUnits, []),
                              l_units(LUnits, []),
                              evaluate(Characteristic, [default(rural)])]),

    context_units:convert(0.01*yr, T1*TUnits, T1),
    context_units:convert(100.0*yr, T2*TUnits, T2),

    T range [T1, T2]^0.8*TUnits,
    (
       Characteristic = rural ->
	 Z mapdot corrosionModel(1000, 0.1, 1.0, LUnits) ~> T
     ;
       Characteristic = marine_mild ->
	 Z mapdot corrosionModel(1000000, 0.01, 1.0, LUnits) ~> T
      ;
       Characteristic = marine_severe ->
	 Z mapdot corrosionModel(1000000, 0.01, 1.0, LUnits) ~> T
      ;
       Characteristic = industrial ->
	 Z mapdot corrosionModel( 800000, 0.01, 1.0, LUnits) ~> T
      ;
       Characteristic = urban ->
	 Z mapdot corrosionModel(10000, 0.05, 1.0, LUnits) ~> T
    ),
    Top mapdot Limit ~> T,
    % context_math:ones_list(T, Limit, [], Top),
    Data tuple T + Z + Top,
    X = 'Time',
    Y = 'Corroded Thickness',
    ZZ = 'Limit',
    XUnits = TUnits,
    YUnits = LUnits,
    get_location(Characteristic, Lat, Lon, Location_Name, Class, EC, Examples),
    reply_html_page([title('Corrosion Models'),
                     \(con_text:style)],
                    [
		     p(i(['Corrosion category : ', EC, ' ', b(Class), ' examples include ', Examples])),
		     \(context_graphing:dygraph_native(Kind, [X, Y, ZZ],
						       [X,XUnits], [Y, YUnits],
						       'Corrosion vs Time', Data)),
		     %, \(con_text:alert('Check Margin', Result ))
		     b([]),
		     hr([]),
		     b([]),
                     \(con_text:inline_button(
		       \(con_text:button_link(
				      'Link to references',
				      '/context_ref_search/search_sweet',
				      render,
				      [[name,'procChemical:Corrosion']]))
				)
                      ),
                     \(con_text:inline_button(
		      \(con_text:button_link('Display Map',
					   '/context_map/navigate',
					   render,
					   [[lat, Lat],
					    [lon, Lon],
					    [title, Location_Name]
					   ]))
					     )
		      ),

                     \(con_text:inline_button(
		       \(con_text:button_link(
				      'Corrosion categories',
				      'corrosion_scale_table',
				      render,
				      []))
				)
                      )

                    ]
		  ).

corrosionModel(Diff, Drag, X0, T*Time, Z*Thickness) :-
   context_units:convert(T*Time, T1*yr, T1),
   Tau is 1 - exp(-T1*Drag),
   Z1 is Diff * Tau/(X0+sqrt(Diff*Tau)),
   context_units:convert(Z1*micron, Z*Thickness, Z), !.
corrosionModel(Diff, Drag, X0, Thickness, T*Time, Z) :-
   corrosionModel(Diff, Drag, X0, T*Time, Z*Thickness).


standard(Names) :-
    findall(p(Name), (rdf(UID, ent:standard, ent:corrosion),
                   rdfS(UID, ent:name, Name)), Names).

scale([Value, Env, Ext, Int, Steel, Zinc, Units]) :-
    rdfS(UID, ent:corrosion_scale, Value),
    rdfS(UID, ent:environmental_conditions, Env),
    rdfS(UID, ent:exterior_examples, Ext),
    rdfS(UID, ent:interior_examples, Int),
    rdfS(UID, ent:carbon_steel_thickness_loss, Steel),
    rdfS(UID, ent:zinc_thickness_loss, Zinc),
    rdfS(UID, ent:unit, Units).


corrosion_scale_table(_Request) :-
    findall(Row,scale(Row), Rows),
    standard(Name),
    reply_html_page([title('Corrosion Scale Table'),
		     \(con_text:style)],
                   [
		    h1('Corrosive Environment Categories'),
		    p(['Several sliding categories of corrosive environments, from ' | Name]),
		    \(con_text:table_multiple_entries(
				   [[b(class), b(env), b(exterior), b(interior),
                                     b('carbon steel'), b(zinc), b(units)]],
				    Rows
						     )
		     )
                   ]).



