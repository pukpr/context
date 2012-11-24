:- module(context_obstacles, [read_rdf/5]).

/** <module> Context obstacles via knowledgebase
    * Reading and rendering tables of deterministic profiles
*/

:- context:register(context_obstacles:navigate).
:- context:register(context_obstacles:read_rdf_data).
:- context:register(context_obstacles:download).
:- context:register(context_obstacles:plot_fft).

:- use_module(context_math).

rdf_data(ID, Name) :-
    rdf(ID, ent:type, literal(obstacle_profile)),
    rdf(ID, ent:name, literal(Name)).

get_all_obstacle_sets(List) :-
    findall(option([value(ID)],[Name]), rdf_data(ID, Name), List).

navigate(Request) :-
   get_all_obstacle_sets(List),
   reply_html_page(
       cliopatria(default),
     [title('Obstacle data sets')],
     [
      \(con_text:table_with_iframe_target(
                                    Request,
		    [h1('Select obstacle for evaluation'),
                      b('Available obstacle profile sets'),
		     form([action('read_rdf_data'), target('target_iframe')],
			 [
			  select([name('data_set')], List),
			  br([]),
			  \(con_text:radio_box_input_two(
					 'plot_scaling',
					 ['linear', 'lin'],
					 ['log scale', 'log']
                                                        )),
			  input([type('submit'), value('Select Data Set')])
			 ]),
		     p(['Obstacle profiles are either single or doubly tracked. Double tracks are shown as alternating colors.']),
		     br([]),
		     br([]),
		     br([]),
		     \(con_text:render_iframe(render))
		    ]
		   )
       )
		]
	       ).


read_rdf(URI, UX, UY, [DataX, DataY, DataZ], 'X,Z1,Z2') :-
    rdfS(URI, ent:units_x, UX),
    rdfS(URI, ent:units_y, UY),
    rdfL(URI, ent:data_x, DataX),
    rdfL(URI, ent:data_y, DataY),
    rdfL(URI, ent:data_z, DataZ), !.

read_rdf(URI, UX, UY, [DataX, DataY, DataY], 'X,Z1,Z2') :-
    rdfS(URI, ent:units_x, UX),
    rdfS(URI, ent:units_y, UY),
    rdfL(URI, ent:data_x, DataX),
    rdfL(URI, ent:data_y, DataY).

read_rdf_data(Request) :-
   http_parameters(Request, [plot_scaling(LL),
                             data_set(URI)
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

   reply_html_page(
       [title('obstacle path')],
       [
                   \(con_text:multi_columns([
			 [
        dl([b(dt('Data Set')),dd(URI),
            b(dt('X units')),dd(X_Units),
            b(dt('Z units')),dd(Z_Units),
            b(dt('log or linear scale')),dd(LL)])
                         ],
                         [
			 \(con_text:table_entries([['course characteristic',Type],
						   ['course description',Desc],
						   ['course material',CM],
						   ['course roughness (in)', CRI],
						   ['dynamic coefficient of friction', FD],
						   ['static coefficient of friction', FS]
						  ]))
                         ]
                                             ])),

        \(con_text:inline_button(
		       \(con_text:button_link(
				      'Display 3D rendering',
				      '/context_render/create',
				      render,
				      [[uri, URI]]))
				)
         ),
        \(con_text:inline_button(
		       \(con_text:button_link(
				      'Display data',
				      'download',
				      render,
				      [[uri, URI],
				       [dl, false]]))
				)
         ),
        \(con_text:inline_button(
		       \(con_text:button_link(
				      'Download data',
				      'download',
				      render,
				      [[uri, URI],
				       [dl, true]]))
				)
         ),
        \(con_text:inline_button(
		       \(con_text:button_link(
				      'Generate PSD',
				      'plot_fft',
				      render,
				      [[uri, URI],
				       [ll, log]]))
				)
         ),

        p([]),
        \(context_graphing:dygraph_plot(LL, Header, UX, UZ,
                                       Heading, Data))
       ]
                  ).


download(Request) :-
    http_parameters(Request, [uri(URI, [string]),
                              dl(DL, [boolean])]),
    read_rdf(URI, _X_Units, _Z_Units, [X,Y,Z], _Header),
    (
    DL=true,
        MIME=[];
    DL=false,
        MIME=mime_type('text/plain')
    ),
    File = './tmp/dl_text',
    tell(File),
    print(data(x(X),y(Y),z(Z))),
    told,
    http_reply_file(
        File,
        [MIME],
        []).


 %        div([style='display:inline-block; clear:both; height:1px;'],


plot_fft(Request) :-
   http_parameters(Request, [uri(URI, [string]),
                              ll(LL, [string])]),
   read_rdf(URI, _X_Units, _Z_Units, [X,Y,_Z], _Header),
/*  reciprocal units
   context_graphing:axis_label('x', X_Units, UX),
   context_graphing:axis_label('z', Z_Units, UZ),
*/
   context_complex:fft_squared(X,Y, XV, YV),
   Profile tuple XV + YV,
   rdfS(URI, ent:name, Name),
   Title = [Name, ' PSD '],
   Headings = ['Sx', 'Data'],
   UX = 'wave number',
   UZ = 'power spectral density',
   reply_html_page([title('obstacle path')],
                   [
                    \(context_graphing:dygraph_native(LL, Headings, UX, UZ, Title, Profile))

                   ]).




