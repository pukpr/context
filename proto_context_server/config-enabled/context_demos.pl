:- module(context_demos, []).

/** <module> Context model demos
    * Terrain Slopes
    * Random Walk
*/

:- context:register(context_demos:navigate).
:- context:register(context_demos:charting).
:- context:register(context_demos:dygraph).
:- context:register(context_demos:gross_terrain).
:- context:register(context_demos:dygraph_test).
:- context:register(context_demos:get_elevation).
:- context:register(context_demos:complex_psd).

:- use_module(context_math).

terrain_selection(Name) -->
   html(form([action(Name),target(target_iframe)],
        [table(
         [tr(
             [td(b('mean')),  td(input([type('text'),name('mean'),value('1.0')])),
              td(table(border='1',
               [tr([td(b('local')), td(input([type('radio'),name('area_scale'),value('local')]))]),
                tr([td(b('wide')),  td(input([type('radio'),name('area_scale'),value('wide' )]))])])),
              td(table(border='1',
               [tr([td(b('pdf')),   td(input([type('radio'),name('query_type'),value('pdf'  )]))]),
                tr([td(b('sample')),  td(input([type('radio'),name('query_type'),value('sample' )]))])])),
              td(input([type('submit')]))
             ]
            )
         ])])).


navigate(_) :-
   reply_html_page(
       cliopatria(default),
       [title('Home')],
       [
         \(con_text:table_with_iframe_target(
                        target_iframe,
         [
           h2('Query Ontology demos'),
           ul([
               li([a([href('gross_terrain?mean=1.0&area_scale=wide&query_type=pdf'),
                      target(target_iframe)],
                     'Call gross terrain'),
                   \(context_demos:terrain_selection('gross_terrain'))
                  ]
                 ),
               li(a([href('sample_data'),
                    target(target_iframe)],
                    'Query terrain sample reference')
                 ),
               li(a([href('/context_r_demo/r_app'), target(target_iframe)],
                    'Use R to generate a PSD')),
               li(a([href('complex_psd?length=1.0'), target(target_iframe)],
                    'Built-in PSD')),
               li(a([href('/context_psd/complex_psd?course=corrugations'),target(target_iframe)],
                    'PSD lookup')),
               li(a([href('/context_random_walk/demo'),target(target_iframe)],
                    'Classic Random walk example using Dynagraph')),
               li(a([href('/context_random_walk/demo_ou'), target(target_iframe)],
                   'Ornstein-Uhlenbeck Random walk example using Dynagraph')),
               li(a([href('/context_random_walk/semi'), target(target_iframe)],
                   'Semi-Markov Random walk example using Dynagraph')),
               li(a([href('/context_random_walk/semi_fft'), target(target_iframe)],
                   'Semi-Markov Random walk FFT example using Dynagraph')),
               li(a([href('charting'), target(target_iframe)],
                     'Random walk example using Google Charts')),
               li(a([href('dygraph'), target(target_iframe)],
                    '2 Random walkers example using Dynagraph')),
               li(a([href('/context_sos/sos'), target(target_iframe)],
                   'Random walk example using sum of sines')),
               li(a([href('/context_sos/sos_config?length=1.0'), target(target_iframe)],
                   'Random walk using sum of sines applied to semi-Markov')),
	       li(a([href='/context_model/apply?model=Canadian_lakes',
                    target=target_iframe],'Example PDF')),
	       li(a([href='/context_demos/get_elevation?lat=44.0&lon=-120.0',
                    target=target_iframe],'Example Elevation')),

               li(a([href('max_density_example'),target(target_iframe)],   'Query data example')),
               li(a([href('find_wind_ref_example'),target(target_iframe)], 'Query wind reference')),
               li(a([href('find_requirement_example'),target(target_iframe)], 'Query requirement')),
               li(a([href('sample_alg_example'),target(target_iframe)], 'Query an algorithm for references')),

               li(h2('Other links')),
               li(a([href('/context_psd_workflow_json/psd_index'),
                     target=target_iframe],
                    'Invoke a workflow to access model PSD artifacts from JSON (deprecated)')),
               li(a([href('/context_obstacles_csv/navigate'),target=target_iframe],
	           'Invoke a workflow to use obstacle artifacts (CSV format deprecated)')),
               li([a(href('/context_ont_utils/navigate'),
                     'Navigate to ontology administration (deprecated)')]),

               li(a([href('dygraph_test'),
                     target=target_iframe], 'Test native dygraph'))
               /*
               li(a(href('/read_tops?plot_scaling=log&file_name=ATC_Perryman_3_logspaced.JSON'),
                    'Read a PSD JSON file'))
               */
              ]
             )
         ]
                                            )
          )
       ]
                  ).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% Slope sampling
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exponential_sample_draw(Result) :-
   random(T),
   log(T,X),
   Result is -X.

exp_slopes(_,R) -->
   {R < 0.0001}.
exp_slopes(M, R) -->
   {P is e**(-R/M),
    R1 is 0.9*R},
   html(tr([td(R), td(P)])),
   exp_slopes(M, R1).

json_complete(x(_,[]), y(_,[],_), In, Out) :-
   atomic_list_concat(['[', In,']'], Out).
json_complete(x(X,S), y(Y,P,Shape), In, Out) :-
   atomic_list_concat(['"',Shape,' ',S,',',P,'"'], Id),
   atomic_list_concat(['[', In, '{"id":',Id,',',X,':',S,',',Y,':',P,'}]'], Out).
json_concat(x(X,S), y(Y,P,Shape), In, Out) :-
   atomic_list_concat(['"',Shape,' ',S,',',P,'"'], Id),
   atomic_list_concat([     In, '{"id":',Id,',',X,':',S,',',Y,':',P,'},'], Out).


exp_slopes_json(X,Y,ID,M,R) -->
   {R < 0.0001,
    P is e**(-R/M),
    R1 is 0.9*R},
   json_complete(x(X,R1), y(Y,P,ID)).
exp_slopes_json(X,Y,ID,M,R) -->
   {P is e**(-R/M),
    R1 is 0.9*R},
   json_concat(x(X,R1),y(Y,P,ID)),
   exp_slopes_json(X,Y,ID, M, R1).



bessel_slopes(_,R) -->
   {R < 0.0001}.
bessel_slopes(M, R) -->
   {X is R/M,
   P is e**(sqrt(sqrt(X/sqrt(10.64+X+1.8/X+0.007631/X/X))) - 2.003*sqrt(X)),
   R1 is 0.9*R},
   html(tr([td(R), td(P)])),
   bessel_slopes(M, R1).



two_random_walker_csv(_,  _, 0, Out, Out).
two_random_walker_csv(Z1,Z2, N, In, Out) :-
   M is N - 1,
   random(X1),
   Y1 is Z1 + X1 - 0.5,
   random(X2),
   Y2 is Z2 + X2 - 0.5,
   atomic_list_concat([In, '+ "', M, ',', Y1, ',', Y2, '\\n"'], Next),
   two_random_walker_csv(Y1, Y2, M, Next, Out).


dygraph(_) :-
   two_random_walker_csv(0.0, 0.0, 1000, '"X, Z1, Z2\\n"', Out),
   reply_html_page([title('chart'),
                    % \(context_graphing:dygraph_script_load),
                    \(con_text:style_cliopatria)
                   ],
                   [
                    \(context_graphing:dygraph_plot(
                                           false,
                                           '',
                                           'surface distance (m)',
                                           'elevation change (m)',
                                           'random walk profile',
                                           Out ))
                   ]).

dygraph_test(_) :-
   X range [1.0,50.0]/0.1,
   Y mapdot sin ~> X,
   Z mapdot cos ~> X,
   Graph tuple X + Y + Z,
   reply_html_page(
                   [title('chart'),
                    \(con_text:style_cliopatria)
                   ],
                   [
                    \(context_graphing:dygraph_native(
                                           false,
                                           ['x', 'y', 'z'],
                                           'x axis',
                                           'y axis',
                                           'test',
                                           Graph))
                   ]).

random_walker(_, 0, In, Out) :-
   atomic_list_concat(['[', In,']'], Out).
random_walker(Z, N, In, Out) :-
   M is N - 1,
   random(X),
   Y is Z + X - 0.5,
   atomic_list_concat([In, ',[', M, ',', Y, ']'], Next),
   random_walker(Y, M, Next, Out).


charting(_) :-
   random_walker(0.0, 1000, '["X", "Z"]', Out),
   reply_html_page(
                   [title('chart'),
                    % \(context_graphing:chart_script_load),
                    \(con_text:style_cliopatria)
                   ],
                   [
                    \(context_graphing:chart_plot(false,
                                                  '"x distance"',
                                                  '"z elevation"',
                                                  '"random walker"',
                                                  Out ))
                   ]).





% %%%
% % 1
gross_terrain_compute(Mean_Slope, 'local', 'pdf', _, _) :-
   A = '"Slope (rise/run)"',
   B = '"Probability"',
   format(atom(S), '~7f Sampled Local Terrain', [Mean_Slope]),
   Variance is 10.0*Mean_Slope,
   exp_slopes_json(A, B, 'o', Mean_Slope, Variance, '', V),
   !,
   reply_html_page([title(S),
                    \(con_text:style_cliopatria)
                    %\(context_graphing:scatter_script_load)
                   ],
                   [
                    \(context_graphing:scatter_plot(0,A,B,V)),
                    table([class('block'),align(center), border(1), width('50%')],
                           [
                            tr([th('Slope'), th('Cumulative Probability')]),
                            \exp_slopes(Mean_Slope, Variance)
                           ])
                   ]).




% %%%
% % 2
gross_terrain_compute(Mean_Slope, 'wide', 'pdf', _, _) :-
   format(atom(S), '~7f Sampled Local Terrain', [Mean_Slope]),
   reply_html_page(
                   [title(S),
                    \(con_text:style_cliopatria)
                   ],
                   [ table([align(center), border(1), width('50%')],
                           [
                            tr([th('Slope'), th('Cumulative Probability')]) |
                            \bessel_slopes(Mean_Slope, 10.0)
                           ])
                   ]).


% %%%
% % 3
gross_terrain_compute(Mean_Slope, 'local', 'sample', _, _) :-
   exponential_sample_draw(X),
   Slope is Mean_Slope*X,
   format(atom(S), '~7f', [Slope]),
   reply_html_page([], title('Sampled Local Terrain'),
                   [ table([class('block'),align(center), border(1), width('50%')],
                           [tr([th('Mean Slope'), th('Sampled Slope')]),
                            tr([td(Mean_Slope), td(S)])])
                   ]).

% %%%
% % 4
gross_terrain_compute(Median_Slope, 'wide', 'sample', _, _) :-
   exponential_sample_draw(X1),
   exponential_sample_draw(X2),
   Slope is Median_Slope*X1*X2,
   format(atom(S), '~7f', [Slope]),
   reply_html_page([], title('Sampled Wide Terrain'),
                   [ table([class('block'),align(center), border(1), width('50%')],
                           [tr([th('Median Slope'), th('Sampled Slope')]),
                            tr([td(Median_Slope), td(S)])])
                   ]).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % Common for 1,2,3,4 choices

gross_terrain(Request) :-
   rdf(realmLandform:'LandSurface', rdf:type, owl:'Class'),
   http_parameters(Request, [mean(Median_Slope),
                             area_scale(Scale),
                             query_type(Query),
                             utm(UTM),
                             seed(Seed)
                             ],
                             [attribute_declarations(param)]),
   gross_terrain_compute(Median_Slope, Scale, Query, UTM, Seed).



get_elevation(Request) :-
   http_parameters(Request, [lat(Lat,[]),
                             lon(Lon,[])]),
   format(atom(S),
          'http://maps.googleapis.com/maps/api/elevation/json?locations=~w,~w&sensor=false',
          [Lat, Lon]),
   http_client:http_get(S, Response, []),
   % print(user_error, Response),
   Response=json([results=[json([elevation=E,
                          location=json([lat=_Lat,
                                         lng=_Lon]),
                          resolution=_])
                   ],
           status='OK']),
   reply_html_page([], title('Elevation'),
                   [ p(E)
                   ]).





% Links to reference servers

:- context:register(context_demos:max_density_example).
:- context:register(context_demos:find_wind_ref_example).
:- context:register(context_demos:find_requirement_example).
:- context:register(context_demos:sample_alg_example).
:- context:register(context_demos:sample_data).


% %%%%%% Example citation and data query %%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

median_slope(0.039).
nominal_mileage(10.0).


sample_data(_) :-
   rdf(realmLandform:'LandSurface', rdf:type, owl:'Class'),
   reference_link('TOC', 'P.531', NamedDest),
   %% Median slope is 0.039
   %% Average Mileage is 10 MPG
   %% Mileage is AvgMileage * (1-sin(Slope))^2
   random(T1), random(T2), log(T1,X1), log(T2,X2),
   median_slope(Median_Slope),
   nominal_mileage(Nominal_MPG),
   Slope is Median_Slope*X1*X2,
   MPG is Nominal_MPG*(1-sin(Slope))^2,
   format(atom(M), '~2f', [MPG]),
   format(atom(S), '~7f', [Slope]),
   format(atom(Title), '~1f ~s', [Nominal_MPG, ' nominal MPG for sampled terrain']),
   reply_html_page([title('Sampled Terrain'),
                    \(con_text:style_cliopatria)],
                   [ h1(Title),
                     table([class('block'),align(center), border(1), width('50%')],
                           [tr([th('Median Slope'), th('Sampled Slope'), th('Average Mileage')]),
                            tr([td(Median_Slope), td(S), td(M)])]),
                     h2(a(href(NamedDest), 'Meta reference'))]).

:- multifile http:location/3.
:- dynamic http:location/3.

http:location(ref, '../../../library/reference', []).

% reference_server('http://wcsn262:5001').
% reference_dir('../../../library/reference/').

reference_name('TOC', 'TheOilConundrum.pdf').
reference_name('META', 'META_Phase_1b_Final_Report_with_Dist_A.pdf').
reference_name('PPthesis', 'pukite_thesis.pdf').


reference_link(Name, Dest, Link) :-
   % reference_server(Server),
   % reference_dir(Dir),
   reference_name(Name, File),
   format(atom(Link), '/ref/foundation/~s#nameddest=~s', [File,Dest]).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%% Domain rules with respect to the data store %%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_maximum_density('material', 1).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%% Example Max Density Query       %%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_density_example(_) :-
   find_maximum_density(Subj, Density),
   reply_html_page(% cliopatria(default),
                   [title('Max Density'),\(con_text:style_cliopatria)],
                   [ h1('Max Density Query'),
                     table([tr([th('Material'), th('Density')]),
                            tr([td(Subj), td(Density)])]) ]).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%% Example Citations/Refs      %%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


find_wind_ref_example(_) :-
   rdf(phenAtmoWind:'Wind', rdf:type, owl:'Class'),
   reference_link('TOC', 'M33.9.wind.speed', NamedDest),
   reply_html_page(%cliopatria(default),
                   [title('Refs'),\(con_text:style_cliopatria)],
                   [ h1('References'),
                     h2(a(href(NamedDest), 'TOC ref')) ]).


find_requirement_example(_) :-
   rdf(humanResearch:'Evidence', rdf:type, owl:'Class'),
   reference_link('META', 'MobilityReq', NamedDest),
   reply_html_page(%cliopatria(default),
                   [title('Reqs'),\(con_text:style_cliopatria)],
                   [ h1('Requirements'),
                     h2(a(href(NamedDest), 'Mobility Req')) ]).

sample_alg_example(_) :-
   rdf(reprSciModel:'Model', rdf:type, owl:'Class'),
   reference_link('PPthesis', 'DiffractionFromSteps', NamedDest),
   reply_html_page(% cliopatria(default),
                   [title('Alg'),\(con_text:style_cliopatria)],
                   [ h1('Models'),
                     h2(a(href(NamedDest), 'Example Derivation')) ]).

% -----------------------------------------
:- use_module(context_complex).


frequencies(S, _, Result, Result) :-
    S < 0.01.
frequencies(S, linear, L, Result) :-
    S1 is 0.95*S,
    frequencies(S1, linear, [S|L], Result).
frequencies(S, log, L, Result) :-
    S1 is 0.95*S,
    LogS is log(S1),
    frequencies(S1, log, [LogS|L], Result).


delayed_exponent(L, Alpha, S, R) :-
    Theta is -L*S,
    Num isx 1.0*exp(i*Theta),
    R isx Num / (Alpha & S).

two_level_model(L1, Alpha1, L2, Alpha2, S, R) :-
    delayed_exponent(L1, Alpha1, S, P),
    delayed_exponent(L2, Alpha2, S, Q),
    One isx 1.0 & 0.0,
    K   isx S   & 0.0,
    R & _ isx (One-P)*(One-Q)/(One-P*Q)/K^2.

two_level_spectrum(_, _, _, _, S, Result, Result) :-
    S < 0.01.
two_level_spectrum(L1, Alpha1, L2, Alpha2, S, L, Result) :-
    two_level_model(L1, Alpha1, L2, Alpha2, S, R),
    S1 is 0.95*S,
    % LogR is log(R),
    LogR is R,
    two_level_spectrum(L1, Alpha1, L2, Alpha2, S1, [LogR|L], Result).

builtin_complex(Length, Scale, F, Result) :-
    S is 100.0,
    Alpha1 is 1.0,
    L1 = Length,
    Alpha2 is 1.0,
    L2 = Length,
    frequencies(S, Scale, [], F),
    two_level_spectrum(L1, Alpha1, L2, Alpha2, S, [], Result).


construct_psd([], [], Out, Out).
construct_psd([F|FR], [R|RR], In, Out) :-
    atomic_list_concat([In, '+ "', F, ',', R, '\\n"'], Next),
    construct_psd(FR, RR, Next, Out).

complex_psd(Request) :-
    http_parameters(Request, [length(Value, [float])]),
    builtin_complex(Value, log, F, Result),
    construct_psd(F, Result, '"S, Intensity\\n"', Out),
    reply_html_page(% cliopatria(default),
                   [title('PSD'),
                    \(con_text:style_cliopatria)
                    % \(context_graphing:dygraph_script_load)
                   ],
                   [
                     \(context_graphing:dygraph_plot( true,
                                                      '',
                                                      'log of wave number',
                                                      'PSD',
                                                      'Constructed PSD from two-level',
                                                      Out ))
                   ]).


