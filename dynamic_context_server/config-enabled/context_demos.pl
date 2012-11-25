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
:- context:register(context_demos:complex_psd_semi_markov).
:- context:register(context_demos:semi).
:- context:register(context_demos:semi_fft).
:- context:register(context_demos:demo).
:- context:register(context_demos:demo_ou).
:- context:register(context_demos:model_format).
:- context:register(context_demos:model_characteristic).
:- context:register(context_demos:model_values).
:- context:register(context_demos:model_index).
:- context:register(context_demos:sos).
:- context:register(context_demos:sos_config).



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


navigate(Request) :-
   reply_html_page(
       cliopatria(default),
       [title('Home')],
       [
         \(con_text:table_with_iframe_target(
                        Request,
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
               li(a([href('complex_psd_semi_markov?course=corrugations'),target(target_iframe)],
                    'PSD lookup')),
               li(a([href('demo'),target(target_iframe)],
                    'Classic Random walk example using Dynagraph')),
               li(a([href('demo_ou'), target(target_iframe)],
                   'Ornstein-Uhlenbeck Random walk example using Dynagraph')),
               li(a([href('semi'), target(target_iframe)],
                   'Semi-Markov Random walk example using Dynagraph')),
               li(a([href('semi_fft'), target(target_iframe)],
                   'Semi-Markov Random walk FFT example using Dynagraph')),
               li(a([href('charting'), target(target_iframe)],
                     'Random walk example using Google Charts')),
               li(a([href('dygraph'), target(target_iframe)],
                    '2 Random walkers example using Dynagraph')),
               li(a([href('sos'), target(target_iframe)],
                   'Random walk example using sum of sines')),
               li(a([href('sos_config?length=1.0'), target(target_iframe)],
                   'Random walk using sum of sines applied to semi-Markov')),
	       li(a([href='/context_model/apply?model=Canadian_lakes',
                    target=target_iframe],'Example PDF')),
	       li(a([href='/context_demos/get_elevation?lat=44.0&lon=-120.0',
                    target=target_iframe],'Example Elevation')),
	       li([\(con_text:gif(workflow)),
		   a(href('model_index'),
		     'Invoke a workflow to access model PDF artifacts (experimental)')]),

               li(a([href('max_density_example'),target(target_iframe)],   'Query data example')),
               li(a([href('find_wind_ref_example'),target(target_iframe)], 'Query wind reference')),
               li(a([href('find_requirement_example'),target(target_iframe)], 'Query requirement')),
               li(a([href('sample_alg_example'),target(target_iframe)], 'Query an algorithm for references')),
	       li([a([href('/context_map/navigate?lat=48.786&lon=9.235&title="Mercedes-Benz TT"'),
			    target(target_iframe)],
                           'Display map')]),
	       li( [
                         \(con_text:form('/context_water/density_test',
				 target_iframe,
				 [[input,'20', 6],
				  [iunits,'c', 6],
				  [ounits,'g/cm^3', 6]]
				)),
                         'units conversion'
                         ]),


               li(h2('Other links')),
               li(a([href('/context_psd_workflow_json/psd_index'),
                     target=target_iframe],
                    'Invoke a workflow to access model PSD artifacts from JSON (deprecated)')),
               li(a([href('/context_obstacles_csv/navigate'),
		     target=target_iframe],
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

complex_psd_semi_markov(Request) :-
    http_parameters(Request, [course(Course, [])]),
    F range [0.002, 2.0]/0.002,
    Out mapdot two_level(Course) ~> F,
    R tuple F + Out,
    reply_html_page(% cliopatria(default),
                   [title('PSD'),
                   \(con_text:style_cliopatria)],
                   [
                     \(context_graphing:dygraph_native( true,
							['x', 'y'],
                                                      'wave number',
                                                      'PSD',
                                                      'PSD from two-level model',
                                                      R))
                   ]).



% ---------------

semi(_) :-
    X range [1,32768]/1,
    Z0 mapdot 0.0 .* X,
    semi_random_walker(X, 1.2, [18.0,3.0], [2.0,2.0], Z0, Z1),
    semi_random_walker(X, 0.6, [20.0,20.0], [12.0,12.0], Z1, Z2),
    semi_random_walker(X, 3.0, [20.0,20.0], [80.0,80.0], Z2, Z3),
    semi_random_walker(X, 1.0, [200.0,200.0], [800.0,800.0], Z3, Z4),
    % context_complex:tuple_list(X, Z4, [], Data),
    % convert X to real scaled value
    Data tuple X + Z4,

    reply_html_page(% cliopatria(default),
                    [title('chart'),\(con_text:style_cliopatria)],
                    [
                     \(context_graphing:dygraph_native(
                                            false,
                                            ['X','Z'],
                                            'surface distance (m)',
                                            'elevation change (m)',
                                            'random walk profile',
                                            Data ))
                    ]).

semi_fft(_) :-
    % X range [1,16384]/1,
    X range [1,32768]/1,
    Z0 mapdot 0.0 .* X,
    %                          L_up L_dn  A_up A_dn
    semi_random_walker(X, 1.2, [18.0,3.0], [2.0,2.0], Z0, Z1),
    semi_random_walker(X, 0.6, [20.0,20.0], [12.0,12.0], Z1, Z2),
    semi_random_walker(X, 3.0, [20.0,20.0], [80.0,80.0], Z2, Z3),
    semi_random_walker(X, 1.0, [200.0,200.0], [800.0,800.0], Z3, Z4),
    context_complex:fft_squared([X, Z4], Array),!,

    reply_html_page(% cliopatria(default),
                    [title('chart'),\(con_text:style_cliopatria)],
                    [
                     \(context_graphing:dygraph_native(
                                            true,
                                            ['Sx','Sz'],
                                            'wavenumber (1/m)',
                                            'PSD (m^2/m)',
                                            'Semi-Markov random walk FFT PSD',
                                            Array ))
                    ]).

% --------------------------------------

demo(_) :-
    X range [1,2000]/1,
    context_random_walk:random_walker(X, 0.0, [], Z),
    reply_html_page(% cliopatria(default),
                    [title('chart'),\(con_text:style_cliopatria)],
                    [
                     \(context_graphing:dygraph_plot(
                                            false,
                                            'X,Z',
                                            'surface distance (m)',
                                            'elevation change (m)',
                                            'random walk profile',
                                            [X,Z] ))
                    ]).

demo_ou(_) :-
    X range [1,2000]/1,
    context_random_walk:ou_random_walker(0.028, 1.0, 0.05, X, Z),
    reply_html_page(% cliopatria(default),
                    [title('OU chart'),\(con_text:style_cliopatria)],
                    [
                     \(context_graphing:dygraph_plot(
                                            false,
                                            'X,Z',
                                            'surface distance (m)',
                                            'elevation change (m)',
                                            'Ornstein-Uhlenbeck random walk profile',
                                            [X,Z] ))
                    ]).


% ---------------------------------



model_index(_) :-
   reply_html_page(
     cliopatria(default),
     title('domains'),
     [h1('Domain Feature'),
      form([action('model_format')], % target('format')],
           [
            select([name('domain')],
               [option([value('slopes')],['terrain slopes']),
                option([value('wind')],  [wind]),
                option([value('lakes')], ['lake sizes']),
                option([value('particles')],['particle sizes']),
                option([value('rain')],  [rainfall]),
                option([value('clutter')],[clutter])]
            ),
            br([]),
            input([type('radio'),name('query_type'),value('pdf')]),b(pdf),
            input([type('radio'),name('query_type'),value('sample')]),b(sample),
            br([]),
            input([type('submit'), value('Select Domain')])
           ])
      ]
     ).


model_format(Request) :-
   http_parameters(Request, [domain(Domain),
                             query_type(Query)
                             ],
                             [attribute_declarations(rdf_load:param)]),
   Query = pdf,
   reply_html_page(
     cliopatria(default),
     title('format'),
     [h1('Format'),
      p(['domain: ', b(Domain)]),
      p(['query:  ', b(Query)]),
      form([action('model_characteristic')], % target('characteristic')],
           [
            select([name('format')],
               [option([value('html')],['HTML table']),
                option([value('xml')],  [xml]),
                option([value('json')], [json]),
                option([value('graph')],[graph]),
                option([value('fmi')],  [fmi])]
            ),
            br([]),
            input([type('checkbox'),name('cdf'),value(true)]),b('Cumulative'),
            input([type('hidden'),name(domain),value(Domain)]),
            input([type('hidden'),name(query_type),value(Query)]),
            input([type('hidden'),name(distribution),value(exponential)]),
            br([]),
            input([type('submit'), value('Select PDF Format')])
           ])

      ]
     ).

model_format(Request) :-
   http_parameters(Request, [domain(Domain),
                             query_type(Query)
                             ],
                             [attribute_declarations(param)]),
   Query = sample,
   reply_html_page(
     cliopatria(default),
     title('format'),
     [h1('Format'),
      p(['domain: ', b(Domain)]),
      p(['query:  ', b(Query)]),
      form([action('model_characteristic')], % target('characteristic')],
           [
            select([name('sampling')],
               [option([value('single')],['single value']),
                option([value('range')], ['range values'])]
            ),
            input([type('hidden'),name(domain),value(Domain)]),
            input([type('hidden'),name(query_type),value(Query)]),
            input([type('hidden'),name(distribution),value(exponential)]),
            br([]),
            input([type('submit'), value('Select Sampling Format')])
           ])

      ]
     ).


model_characteristic(Request) :-
   http_parameters(Request, [domain(Domain),
                             query_type(Query),
                             cdf(CDF),
                             format(Format),
                             distribution(Distribution)
                             ],
                             [attribute_declarations(param)]),
   Query = pdf,
   reply_html_page(
     cliopatria(default),
     title('Characteristic'),
     [table([border=0, width('100%'), height('600')],
	    [tr([td([width('20%'),height('100%'), valign(top)],
							       [
      h1('Characteristic'),
      p(['domain:       ', b(Domain)]),
      p(['query:        ', b(Query)]),
      p(['format:       ', b(Format)]),
      p(['cumulative:   ', b(CDF)]),
      p(['distribution: ', b(Distribution)]),
      form([action('gross_terrain'), target('results')],
           [
            input([type('hidden'),name(query_type),value(Query)]),
            input([type('hidden'),name(mean),value(1.0)]),
            input([type('hidden'),name(area_scale),value(local)]),
            input([type('hidden'),name(utm),value("none")]),
            input([type('hidden'),name(seed),value(1.0)]),
            br([]),
            input([type('submit'), value('Generate Distribution')])
           ])
				]),
			     td([iframe([name(results),
					 width('100%'),
					 height('100%')
					],
					[])])
			    ]
			   )

			]
	   )
      ]
     ).


model_characteristic(Request) :-
   http_parameters(Request, [domain(Domain),
                             query_type(Query),
                             sampling(Sampling),
                             distribution(Distribution)
                             ],
                             [attribute_declarations(param)]),
   Query = sample,
   reply_html_page(
     cliopatria(default),
     title('Characteristic'),
       [table([border=0, width('100%'), height('600')],
	      [tr([td([width('20%'),height('100%'), valign(top)],
		      [h1('Characteristic'),
		       p(['domain:       ', b(Domain)]),
		       p(['query:        ', b(Query)]),
		       p(['sampling:     ', b(Sampling)]),
		       p(['distribution: ', b(Distribution)])
		      ]
		     ),
		   td([iframe([name(results),
			       width('100%'),
			       height('100%')
			      ],
			      [])])
		  ]
		 )
	      ]
	     )
       ]
     ).

% --------------------------

generate_random_walk_1(X, Z) :-
   % Real space
   linear_range(0, 999, Dx),
   X mapdot 0.01 .* Dx,

   % Reciprocal space
   % Create a log frequency range and apply a PSD & Phase
   % Apply sqrt to intensity density
   log_range(0.01, 10.0, 0.6, Sx),
   PSD mapdot power_law_2_pdf(100.0) ~> Sx,
   Phi mapdot random_phase ~> Sx,
   Amp mapdot sqrt ~> PSD * Sx,
   context_sos:sos(X, Sx, Phi, Amp, [], Z).

generate_random_walk_2(Period, X, Z) :-
   % Real space
   linear_range(0, 999, Dx),
   X mapdot 0.01 .* Dx,

   % Reciprocal space
   builtin_complex(Period, linear, Sx, PSD),
   Phi mapdot random_phase ~> Sx,
   Amp mapdot sqrt ~> PSD * Sx,
   context_sos:sos(X, Sx, Phi, Amp, [], Z).

% param(length, [float]).

sos(Request) :-
   http_parameters(Request, []),
   generate_random_walk_1(X, Z),
   construct_psd(X, Z, '"X, Z\\n"', Out),
   reply_html_page(% cliopatria(default),
                   [title('Superposition of sines from PSD'),
                    \(con_text:style_cliopatria)
                    % \(context_graphing:dygraph_script_load)
                   ],
                   [
                    \(context_graphing:dygraph_plot( false,
                                                     '',
                                                     'x',
                                                     'z',
                                                     'Superposition of sines from PSD',
                                                     Out ))
                   ]).

sos_config(Request) :-
   http_parameters(Request, [length(Length, [float])]),
   generate_random_walk_2(Length, X, Z),
   construct_psd(X, Z, '"X, Z\\n"', Out),
   reply_html_page(% cliopatria(default),
                   [title('Superposition of sines from semi-Markov PSD'),
                    \(con_text:style_cliopatria)
                    % \(context_graphing:dygraph_script_load)
                   ],
                   [
                    \(context_graphing:dygraph_plot(false,
                                                    '',
                                                    'x',
                                                    'z',
                             'Superposition of sines from semi-Markov PSD',
                                                    Out ))
                   ]).

