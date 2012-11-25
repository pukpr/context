:- module(context_profile, [sync/3,
			    hat/4]).

/** <module> Context profiles
    * Reading and rendering tables of deterministic profiles
    * FFT PSD
*/

:- context:register(context_profile:navigate).
:- context:register(context_profile:process_profile).

:- use_module(context_complex).
:- use_module(context_math).
:- use_module(context_psd).

get_all_course_files(List) :-
    findall(option([value(URI)],[Name]),
	    (	rdf(URI, ent:type, literal(terrain_profile)),
		rdf(URI, ent:name, literal(Name))
	    ),
	    List).

navigate(Request) :-
   get_all_course_files(List),
   reply_html_page(
       cliopatria(default),
       [title('Course daya sets')],
       [
	\(con_text:table_with_iframe_target(
		       Request,
			[h1(b(i('Choose Course Profile'))),
			 form([action('process_profile'), target('target_iframe')],
                          [select([name('file_name')], List),
			   input([type('submit'), value('Plot characteristic')]),
                           h2('Evaluate:'),
                           \(con_text:radio_box_input_two(
					  'characteristic',
					  ['power spectral density (PSD)', 'psd'],
					  ['micro profile', 'profile']
							 )),
                           hr([]),
                           h2('Scaling:'),
                           \(con_text:radio_box_input_two(
					  'plot_scaling',
					  ['log (best for PSD)', 'log'],
					  ['linear (best for profile)', 'lin'])),
                           hr([])
                           % input([type('submit'), value('Plot characteristic')])
                          ]),
			 \(render_iframe(render) )

			]
					   )
	 )
       ]
		  ).


% Monte carlo generated from semiMarkov

% This is dependent on discretized steps not on dimensional scale
window_filter(0, X, X) :- !.
window_filter(N, X, Z) :-
    uniform(N, W),
    Z window X/W.

mc_model_component([], _, WW, WW).
mc_model_component([F|R], XX, W, WW) :-
    two_level_random_walk(F, XX, W, W1),!,
    mc_model_component(R, XX, W1, WW).

mc_model(List, Filter, X, WWW) :-
    W mapdot 0.0 .* X,
    mc_model_component(List, X, W, WW),!,
    window_filter(Filter, WW, WWW).

% PSD model for semiMarkov

% Goes to context_functions
sync(W, S, Result) :-
   Result is (sin(W*S)/(W*S))^2.
hat(W, Scale, S, Result) :-
   Result is Scale/(1+(W*S)^2).

semi_markov_model_component([], _, WW, WW).
semi_markov_model_component([F|R], X, W, WW) :-
    W1 mapdot W + (two_level(F) ~> X), !,
    semi_markov_model_component(R, X, W1, WW).

semi_markov_model(List, Weight, N, % XV,
		  SX, Filtered) :-
    W mapdot 0.0 .* SX,
    semi_markov_model_component(List, SX, W, Model), !,
    %length(SX, L),
    Integral integrate Model * SX,
    last(Integral, Int),
    Scaling is Weight*sqrt(2*pi)/Int,
    Window is 0.0016*N,  % magic number
    Filtered mapdot Model * (hat(Window, Scaling) ~> SX).


process_profile(Request) :-
   http_parameters(Request, [plot_scaling(LL),
                             file_name(URI),
                             characteristic(Character)
                             ],
                             [attribute_declarations(param)]),
   rdfS(URI, ent:name, U),
   rdfR(URI, ent:lat, Lat),
   rdfR(URI, ent:lon, Lon),
   rdfS(URI, ent:units_x, X),
   rdfS(URI, ent:units_y, Z),
   rdfL(URI, ent:data_x, XX),
   rdfL(URI, ent:data_y, ZZ),
   rdfI(URI, ent:filter, FilterNum),
   rdfL(URI, ent:model, Model_List),
   rdfR(URI, ent:weight, Weight),

   print(user_error, [U, Model_List]),
   % Generate a Monte Carlo run to compare against
   context_complex:fft_pad(XX, PadX),
   mc_model(Model_List, FilterNum, PadX, MC_Results), !,
   print(user_error, ['MC finished']),

   (
        Character = profile
   ->
        MC shrink MC_Results/XX,
        ZD unbias ZZ,
        Profile tuple XX + ZD + MC,
        Title = 'Terrain profile',
	Headings = ['x', 'z', 'MonteCarlo'],
        context_graphing:plain_axis_label('x', X, UX),
        context_graphing:plain_axis_label('z', Z, UZ)
   ;
        % PSD of Data
        context_complex:fft_squared(XX, ZZ, SX, PSD),
	% PSD of MC model of data
        context_complex:fft_squared(PadX, MC_Results, _SX_MC, PSD_MC),

        % PSD Model of Data
	semi_markov_model(Model_List, Weight, FilterNum, SX, PSD_Model),
        print(user_error, ['Markov finished']),

        % Combination
        Profile tuple SX + PSD + PSD_Model + PSD_MC,
        print(user_error, ['tuple ready']),

        Title = 'Power Spectral Density',
	Headings = ['Sx', 'Data', 'Model', 'MonteCarlo'],
        UX = 'wave number (radians/m)',
        UZ = 'psd (m^2)/(1/m)'
   ),
   reply_html_page([title('terrain profile')],
                   [
                    dl([b(dt('Data')),dd(U),
                        b(dt('X units / Z units')),dd([X, ' / ', Z]),
                        b(dt('Latitude / Longitude')),dd([Lat, ' / ', Lon]),
                        b(dt('Components')),dd(
                                                \(con_text:space_list(Model_List))
                                              )
                       ]
                      ),

                    \(con_text:button_link('Display Map',
					   '/context_map/view',
					   render,
					   [[lat, Lat],
					    [lon, Lon],
					    [title, U]
					   ])),

                    \(context_graphing:dygraph_native(LL, Headings, UX, UZ, Title,
						      Profile))
                   ]).







