:- module(context_obstacles_csv, [ ]).

/** <module> Context obstacles from CSV files
    * Reading and rendering tables of deterministic profiles
    * FFT PSD
*/

:- context:register(context_obstacles_csv:navigate).
:- context:register(context_obstacles_csv:read_csv).

:- use_module(context_complex).
:- use_module(context_math).
:- use_module(context_psd).

get_all_obstacle_files(List) :-
    findall(option([value(FilePath)],[Name]),
	    (
	    rdf(URI, ent:src_file, literal(FilePath)),
	    rdf(URI, ent:name, literal(Name))
	    ), List).

navigate(_) :-
   get_all_obstacle_files(List),
   reply_html_page(
       cliopatria(default),
       [title('CSV files')],
       [table(
            [tr([class(buttons)],
                 [th([valign(top), align(left)],
                      [h1(b(i('Choose Obstacle'))),
                     form([action('read_csv'), target('results')],
                          [select([name('file_name')], List),
                           hr([]),
                           \(con_text:radio_box_input_two('characteristic',
							  ['profile', 'profile'],
							  ['PSD', 'psd'])),
                           hr([]),
                           \(con_text:radio_box_input_two('plot_scaling',
							  ['linear', 'lin'],
							  ['log base 10', 'log'])),
                           hr([]),
                           input([type('submit'), value('Plot characteristic')])
                          ])
                    ]
                   ),
                 td([iframe([name('results'),
                             align('right'),
                             width(600),
                             height(600)
                            ], [p(status)])])
                ]
               )
            ]
             )
       ]
                  ).


headings(2, ['x','y']).
headings(3, ['x','y','z']).

read_csv(Request) :-
   http_parameters(Request, [plot_scaling(LL),
                             file_name(FN),
                             characteristic(Character)
                             ],
                             [attribute_declarations(param)]),

   context_file_reading:read_tabbed_csv(FN, X, Z, Arity, P),
   (
        Character = profile
   ->
        Title = 'profile',
        Profile = P,
        headings(Arity, Headings),
        context_graphing:axis_label('x', X, UX),
        context_graphing:axis_label('z', Z, UZ)
   ;
	% PSD of Data
        Title = 'PSD',
        context_complex:fft_squared(P, XV, YV),
        Profile tuple XV + YV,
	Headings = ['Sx', 'Data'],
        UX = 'wave number',
        UZ = 'psd'
   ),
   reply_html_page([title('obstacle path')],
                   [
                    dl([b(dt('File')),dd(FN),
                        b(dt('X units')),dd(X),
                        b(dt('Z units')),dd(Z),
                        b(dt('log or linear scale')),dd(LL)]),
                    \(context_graphing:dygraph_native(LL, Headings, UX, UZ, Title,
						      Profile))
                   ]).
