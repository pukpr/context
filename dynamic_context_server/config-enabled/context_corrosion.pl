:- module(context_corrosion, [
			      corrosionModel/6,
                              materials/3
			     ]).

/** <module> Models of corrosion
    * Corrosive growth
    * Categories of corrosion environments
    *
*/

:- use_module(context_math).
% :- use_module(library(dialect/ifprolog)).

:- context:register(context_corrosion:navigate).
:- context:register(context_corrosion:plot).
:- context:register(context_corrosion:corrosion_scale_table).
:- context:register(context_corrosion:corrosion_components).


%%   collect_component_materials(+Metal, -List)
%
%    Collect component materials
collect_component_materials(Metal, List) :-
    findall([Name],
            (   rdfS(Metal_Resource, rdfs:label, Metal),
                rdf(Metal_Resource, rdfs:subClassOf, Component),
                rdfS(Component, rdfs:label, Name)),
	    /*
	        (   % atom_split(Name, '-', [T1,T2,T3])
		    atom_to_term(Name, [T1, -, T2, -, T3], [])
		 ->
		    true
		;
		    T1 = Name,
		    T2 = '',
		    T3 = ''
		)
            ),
	    */
	    List).

%%   materials(+Query, -Resource, -Title)
%
%    Lookahead on materials
materials(Query, Resource, Title) :-
    rdf(Resource, rdfs:label,  literal(substring(Query),Title)),
    rdf(Resource, rdfs:subClassOf, _Component).

%%   corrosion_components(+Request)
%
%    Corrosion components
corrosion_components(Request) :-
    http_parameters(Request, [mat(Material, [])]),
    upcase_atom(Material, Mat),
    collect_component_materials(Mat, List),
    reply_html_page([title('Corrosion Components'),
                     \(con_text:style)],
                    [
		     \(con_text:table_multiple_entries([[Mat]], List))
		    ]).



%%   navigate(+Request)
%
%    Dynamic page to corrosion models
navigate(Request) :-
   collect_unit_options(length, Lunits),
   collect_unit_options(time, Tunits),

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
		      i('available materials:'),
		      % \(con_text:form(corrosion_components, render, [['mat', 'IRON']])),
                      %%%%%%%%%%%%%%%%
		      \(con_text:form_ac(corrosion_components, render, materials, mat)),
                      % \(con_text:autoc(materials, mat)),
                      %%%%%%%%%%%%%%%%%%%

                      % \(button_link('Component Types', corrosion_components, render)),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).

%%   check_margin(+Z, +Limit, -Failed)
%
%    Check if within margin
check_margin(Z, Limit, 'Margin failed') :-
    last(Z, Last),
    Last < Limit,
check_margin(_Z, _Limit, 'Margin OK').

%%   get_location(+Characteristic, +Lat, +Lon, +Location_Name, +Class, +EC, -Examples)
%
%    Get location of the empirical data set
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

%%   corrosionModel(+Diff, +Drag, +X0, +ThicknessUnits, +Time, -Z)
%
%    Corrosion growth model
corrosionModel(Diff, Drag, X0, T*Time, Z*Thickness) :-
   context_units:convert(T*Time, T1*yr, T1),
   Tau is (1 - exp(-T1*Drag))/Drag,
   % Z1 is Diff * Tau/(X0+sqrt(Diff*Tau)),
   Z1 is sqrt(Diff*Tau)*sqrt(Tau/X0)/(1+sqrt(Tau/X0)),
   context_units:convert(Z1*micron, Z*Thickness, Z), !.
corrosionModel(Diff, Drag, X0, Thickness, T*Time, Z) :-
   corrosionModel(Diff, Drag, X0, T*Time, Z*Thickness).

% Tau = (1-EXP(-f*t))/f
% k*SQRT(D*Tau)*SQRT(Tau/X)/(1+SQRT(Tau/X))

%%   plot(+Request)
%
%    Plot corrosion curvepage
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
	 Z mapdot corrosionModel(14000, 0.2, 150.0, LUnits) ~> T
     ;
       Characteristic = marine_mild ->
	 Z mapdot corrosionModel(18000, 1e-8, 2.0, LUnits) ~> T
      ;
       Characteristic = marine_severe ->
	 Z mapdot corrosionModel(65000, 1e-8, 2.0, LUnits) ~> T
      ;
       Characteristic = industrial ->
	 Z mapdot corrosionModel(40000, 1e-8, 3.0, LUnits) ~> T
      ;
       Characteristic = urban ->
	 Z mapdot corrosionModel(14000, 0.4, 2.0, LUnits) ~> T
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
					   '/context_map/view',
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


%%   standard(-Names)
%
%    Name of ISO standard corrosion docs
standard(Names) :-
    findall(p(Name), (rdf(UID, ent:standard, ent:corrosion),
                   rdfS(UID, ent:name, Name)), Names).

%%   scale(-Tuple)
%
%    Scale info as tuple set
scale([Value, Env, Ext, Int, Steel, Zinc, Units]) :-
    rdfS(UID, ent:corrosion_scale, Value),
    rdfS(UID, ent:environmental_conditions, Env),
    rdfS(UID, ent:exterior_examples, Ext),
    rdfS(UID, ent:interior_examples, Int),
    rdfS(UID, ent:carbon_steel_thickness_loss, Steel),
    rdfS(UID, ent:zinc_thickness_loss, Zinc),
    rdfS(UID, ent:unit, Units).


%%   corrosion_scale_table(+Request)
%
%    Corrosion scale table page
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



