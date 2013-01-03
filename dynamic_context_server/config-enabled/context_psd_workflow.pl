:- module(context_psd_workflow, []).

/** <module> Plotting PSD artifacts via knowledgebase
    * Reading and rendering tables of power spectral density plots
*/

:- context:register(context_psd_workflow:navigate).
:- context:register(context_psd_workflow:read_rdf_data).
:- context:register(context_psd_workflow:simulate_walk).
:- context:register(context_psd_workflow:soil_display).

:- use_module(context_complex).
:- use_module(context_math).
:- use_module(context_functions).

%%   rdf_data(+ID, -Name)
%
%    Local RDF data
rdf_data(ID, Name) :-
    rdf(ID, ent:type, literal(terrain_psd)),
    rdf(ID, ent:name, literal(Name)).

%%   get_all_psd_sets(-List)
%
%    Get all PSD sets
get_all_psd_sets(List) :-
    findall(option([value(ID)],[Name]), rdf_data(ID, Name), List).

%%   navigate(+Request)
%
%    Dynamic page to indexed fine-terrain power spectral densities
navigate(Request) :-
   get_all_psd_sets(List),
   reply_html_page(
     cliopatria(default),
     [title('PSD data sets')],
     [\(con_text:table_with_iframe_target(
		    Request,
		     [h1('Select test course for evaluation'),
                      b('PSD Courses Available'),
		      form([action('read_rdf_data'), target('target_iframe')],
			 [
			  select([name('data_set')], List),
			  br([]),
			  \(con_text:radio_box_input_two('plot_scaling',
							 ['log scale', 'log'],
							 ['linear', 'lin'])),
			  br([]),

			  \(con_text:radio_box_input_two('plot_type',
							 ['static plot',  'scatter'],
							 ['zoomable plot', 'dynamic'])),
			  br([]),
			  input([type('submit'), value('Plot Data Set')])
			 ]),
                      % p('For each course, identifying characteristics are shown along with a PSD plot of the course profile'),
		      br([]),

                      \(con_text:render_iframe(render))

		      /* iframe([name(render),
			     width('100%'),
			     height('350')
			    ],
			    []) */
		     ]
					 ))
     ]
		  ).


%%   read_rdf(+URI, +UX, +UY, +XY, +Axis)
%
%    Read PSD from RDF
read_rdf(URI, UX, UY, [DataX, DataY], 'X,Z') :-
    rdfS(URI, ent:units_x, UX),
    rdfS(URI, ent:units_y, UY),
    rdfL(URI, ent:data_x, DataX),
    rdfL(URI, ent:data_y, DataY).

%%   reference_name(+Ref, -File)
%
%    Reference name giving file
reference_name('TOPS1', '1-1-010_Vehicle_Test_Course_Severity__Surface_Roughness.pdf').

%%   reference_link(+Name, -Dest, -Link)
%
%    Reference Link for PSD data
reference_link(Name, Dest, Link) :-
   % reference_server(Server),
   % reference_dir(Dir),
   reference_name(Name, File),
   format(atom(Link), '/ref/~s#nameddest=~s', [File,Dest]).

%%   read_rdf_data(+Request)
%
%    Process specific PSD data set
read_rdf_data(Request) :-
   http_parameters(Request, [plot_scaling(LL),
                             data_set(URI),
                             plot_type(Plot)
                             ],
                             [attribute_declarations(param)]),
   read_rdf(URI, X_Units, Z_Units, Data, Header),
   context_graphing:axis_label('x', X_Units, UX),
   context_graphing:axis_label('z', Z_Units, UZ),
   context:rdf_default(URI, ent:course_description, Desc, '(empty)'),
   context:rdf_default(URI, ent:course_material, CM, '(empty)'),
   context:rdf_default(URI, ent:course_roughness_inches, CRI, '(empty)'),
   context:rdf_default(URI, ent:'F_dynamic', FD, '(empty)'),
   context:rdf_default(URI, ent:'F_static', FS, '(empty)'),
   context:rdf_default(URI, ent:type, Type, '(empty)'),
   context:rdf_default(URI, ent:name, Heading, '(empty)'),
   context:rdf_default(URI, ent:docref, Docref, '(empty)'),
   context:rdf_default(URI, ent:soil_classification, Soil, '(empty)'),
   reference_link('TOPS1',Docref, Dlink),
   reply_html_page([title('PSD plots')],
                   [\(con_text:multi_columns([
                                   [
                                   dl([b(dt('Data Set')),dd(\(con_text:uri_index_link(URI))),
                                        b(dt('X units')), dd(X_Units),
                                        b(dt('Z units')), dd(Z_Units),
                                        b(dt('log or linear scale')),dd(LL)])
                                   ],
                                   [
                                    \(con_text:table_entries([['course characteristic',Type],
                                                              ['course description',Desc],
                                                              ['course material',CM],
                                                              ['course roughness (in)', CRI],
                                                              ['dynamic coefficient of friction', FD],
                                                              ['static coefficient of friction', FS],
                                                              ['soil types', a([href=['soil_display?soil=',Soil],
                                                                               target=render],
                                                                               Soil)]
                                                             ])
                                     )
                                   ]
                                             ]
                                            )
                     ),
                    \(con_text:inline_button(
                                   \(con_text:button_link(
                                                  'Display Simulated Profile',
                                                  'simulate_walk',
                                                  render,
                                                  [[data_set, URI]]))
                                            )),
                    \(con_text:inline_button(
                                   \(con_text:button_link(
                                                  'TOPS Reference',
                                                  Dlink,
                                                  render,
                                                  [[data_set, URI]]))
                                            )),
                    hr([]),
                    \(context_graphing:plot(Plot, LL, Header, UX, UZ, Heading, Data))
                     ]
                  ) .

/*
sos([], _, _, _, Final, Final).
sos([X|Rest], Sx, Phi, Amp, Initial, Final) :-
   Data mapdot sin ~> (X .* Sx + Phi),
   Z dot Data * Amp,
   !,
   sos(Rest, Sx, Phi, Amp, [Z|Initial], Final).
*/


%%   simulate_walk(+Request)
%
%    Monte carlo simulation from PSD data set
simulate_walk(Request) :-
   http_parameters(Request,
		   [data_set(URI)],
		   [attribute_declarations(param)]),
   read_rdf(URI, _UX, _UY, [SX, SY], Header),
   length(SX, N),
   length(SY, M),
   X range [1,N]/1,
   print(user_error, [N,M]),!,
   % Y = X,
   generate_sos_profile(X, psd(SX, SY), Y),
   reply_html_page([title(sos)],
		   [
		    \(context_graphing:plot(dynamic, lin, Header, 'x', 'y',
                                            'Simulated profile via Fourier series', [X, Y]))
		   ]).

        /*
        % PSD of Data for SOS
        generate_sos_random_walk(XX, XV, YV, SSRW),
        Profile tuple XX + ZZ + WW + SSRW,
        */


%%   soil_display(+Request)
%
%    Display soil categories
soil_display(Request) :-
   http_parameters(Request,
		   [soil(Soil, [])]
                  ),

   context_soil:parse_request(Soil, Soil_Text),

   reply_html_page([title(soil),
                   \(con_text:style)],
		   [
                    h2('Contains the following soil types:'),
		    ol(Soil_Text),
                    a([href='/context_soil/soil_table'], i('[USCS soil table]'))
		   ]).


