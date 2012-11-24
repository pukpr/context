:- module(context_climate_AR7038, []).

:- context:register(context_climate_AR7038:navigate).
:- context:register(context_climate_AR7038:plot).

:- use_module(context_math).



list_climate_design_types(Target) -->
    {findall(option([value(Name)],[Name]),
                    (rdf_(ent:ar7038_climate, ent:climate_design_type, ID),
                     rdf_(ID, ent:name, Name)), Selects)},
    html(
        form([action(plot), target(Target)],
			 [
			  select([name('climate_type')], Selects),
			  %  br([]),
			  % \(con_text:radio_box_input_two( 'storage', ['environment', 1], ['storage', 0])),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['temperature', 'ent:hourly_temperature'],
					 ['humidity', 'ent:hourly_relative_humidity'],
					 ['solar', 'ent:solar_radiation'],
					 ['storage temperature', 'ent:storage_temperature'],
					 ['storage humidity', 'ent:storage_humidity']
                                         ])),

                          input([type('submit'), name(kind), value('Discrete')]),
			  input([type('submit'), name(kind), value('Interpolate')]),
			  input([type('submit'), name(kind), value('Filter')])
			 ]
            )
        ).


navigate(Request) :-
   rdf_(ent:ar7038_spec, ent:description, Description),
   reply_html_page(cliopatria(default),
                   [title('AR 70-38')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Environmental specifications'),
                      p(Description),
                      ul(li(['Select Climate type',
                             \(context_climate_AR7038:list_climate_design_types(target_iframe))
                            ]
                           ))
                     ,
                      br([]),
		      \(con_text:render_iframe(render))
                     ]
                                         ))
     ]
		  ).


plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
                              climate_type(Climate_Name, []),
                              evaluate(Characteristic, [])]),

    rdf_(ent:ar7038_spec, ent:path, Path),

    % context:create_global_term(Characteristic, Ch),
    atom_to_term(Characteristic, Ch, []),
    % Ch = Characteristic,
    rdf_(ID, ent:name, Climate_Name),
    rdf_(ent:ar7038_climate, ent:climate_design_type, ID),
    rdf_(ent:ar7038_spec,  ent:hours, X),
    rdf_(ID, Ch, Y),
    rdf_(ID, ent:pageref, SpecPage),
    rdf_(ID, ent:qstag360equivalent, QSTAG),
    rdf_(Ch, ent:unit, Units),
    rdf_(Units, ent:description, Unit),
    (Kind = 'Interpolate' ->
        % create a wrap-around of three days
        Days cat [Y,Y,Y],
        Day range [24.0,48.0]/0.1,
        ADay range [0.0,24.0]/0.1,
        % decimate_curve(Day, Days, Output),
        Output mapdot lookup_table(1.0, 3.0, Days) ~> Day,
        Data tuple ADay + Output
    ;
        Kind = 'Filter' ->
        Days cat [Y,Y,Y],
        Output window Days * [0.1, 0.3, 0.5, 0.3, 0.1],
        T1 offset Output - 24,  % lop off the first 24
        Slice shrink T1/X,
        Data tuple X + Slice
    ;
        Data tuple X + Y
    ),
    reply_html_page([title('AR 70-38'),
                     \(con_text:style)],
                    [
                     \(context_graphing:dygraph_native(lin, [time,Unit], 'hour', [Characteristic,Unit],
                                                       [Characteristic, Climate_Name],
                                                       Data)),
                     \(con_text:inline_button(
		       \(con_text:button_link(
				      'Link to specifications',
                                      Path+SpecPage,
				      % '/ref/AR-70-38.html#p02-2',
				      render,
				      []))
				)
                      ),
                     br([]),
                     br([]),
                     br([]),
                     br([]),
                     br([]),
                     \(con_text:alert('QSTAG 360 eqivalent', 'QSTAG = '+QSTAG))
                     % a([href='#', onclick='alert("' + QSTAG +'");'], 'QSTAG 360 eqivalent')

                    ]
		  ).


rdf_(ent:ashrae, ent:solarLoad, not_completed).

rdf_(ent:ar7038_spec, ent:description, 'Army Regulation 70-38, "Research, Development, Test and Evaluation of Materiel for Extreme Climatic Conditions", 9/15/1979').
rdf_(ent:ar7038_spec, ent:path, '/ref/AR-70-38.html#').

rdf_(ent:ar7038_spec, ent:context_type, ent:ar7038_climate).
rdf_(ent:ar7038_spec, ent:context_type, ent:ar7038_environment).

rdf_(ent:ar7038_climate, ent:climate_design_type, ent:hotDry).
rdf_(ent:ar7038_climate, ent:climate_design_type, ent:hotHumid).
rdf_(ent:ar7038_climate, ent:climate_design_type, ent:constantHighHumidity).
rdf_(ent:ar7038_climate, ent:climate_design_type, ent:variableHighHumidity).
rdf_(ent:ar7038_climate, ent:climate_design_type, ent:basicHot).
rdf_(ent:ar7038_climate, ent:climate_design_type, ent:basicCold).
rdf_(ent:ar7038_climate, ent:climate_design_type, ent:cold).
rdf_(ent:ar7038_climate, ent:climate_design_type, ent:severeCold).

rdf_(ent:hotDry, ent:description, 'hot-dry, QSTAG 360 = A1 (north Africa, Middle East, Pakistan, India, southwest US, northern Mexico)').
rdf_(ent:hotHumid, ent:description, 'hot-humid, QSTAG 360 = B3 (Persian Gulf, Red Sea)').
rdf_(ent:constantHighHumidity, ent:description, 'constant high humidity, QSTAG 360 = B1').
rdf_(ent:variableHighHumidity, ent:description, 'variable high humidity, QSTAG 360 = B2').
rdf_(ent:basicHot, ent:description, 'basic hot, QSTAG 360 = A2 (US, Mexico, Africa, Asia, Australia, South America, southern Spain, SW Asia)').
rdf_(ent:basicCold, ent:description, 'basic cold, QSTAG 360 = C1 (high latitude coasts, eg, southern Alaska)').
rdf_(ent:cold, ent:description, 'cold, QSTAG 360 = C2 (Canada, Alaska, Greenland, northern Scandinavia, northern Asia, Tibet, Alps, Himalayas, Andes)').
rdf_(ent:severeCold, ent:description, 'severe cold, QSTAG 360 = C3 (interior Alaska, Canadian Yukon, Greenland icecap, north Asia)').

rdf_(ent:hotDry, ent:name, 'hot-dry').
rdf_(ent:hotHumid, ent:name, 'hot-humid').
rdf_(ent:constantHighHumidity, ent:name,  'constant high humidity').
rdf_(ent:variableHighHumidity, ent:name, 'variable high humidity').
rdf_(ent:basicHot, ent:name, 'basic hot').
rdf_(ent:basicCold, ent:name, 'basic cold').
rdf_(ent:cold, ent:name, 'cold').
rdf_(ent:severeCold, ent:name, 'severe cold').

rdf_(ent:hotDry, ent:pageref, 's12-4a').
rdf_(ent:hotHumid, ent:pageref, 's12-4b').
rdf_(ent:constantHighHumidity, ent:pageref,  's22-5a(2)').
rdf_(ent:variableHighHumidity, ent:pageref, 's22-5a(3)').
rdf_(ent:basicHot, ent:pageref, 's12-5b').
rdf_(ent:basicCold, ent:pageref, 's12-5c').
rdf_(ent:cold, ent:pageref, 'p02-6').
rdf_(ent:severeCold, ent:pageref, 'p02-7').

rdf_(ent:hotDry, ent:qstag360equivalent, 'A1').
rdf_(ent:hotHumid, ent:qstag360equivalent, 'B3').
rdf_(ent:constantHighHumidity, ent:qstag360equivalent, 'B1').
rdf_(ent:variableHighHumidity, ent:qstag360equivalent, 'B2').
rdf_(ent:basicHot, ent:qstag360equivalent, 'A2').
rdf_(ent:basicCold, ent:qstag360equivalent, 'C1').
rdf_(ent:cold, ent:qstag360equivalent, 'C2').
rdf_(ent:severeCold, ent:qstag360equivalent, 'C3').

rdf_(ent:ar7038_spec, ent:condition_type, ent:operational).
rdf_(ent:ar7038_spec, ent:condition_type, ent:storageTransit).


rdf_(ent:hourly_temperature, ent:unit, ent:f).
rdf_(ent:hourly_relative_humidity, ent:unit, ent:percent).
rdf_(ent:storage_temperature, ent:unit, ent:f).
rdf_(ent:storage_humidity, ent:unit, ent:percent).
rdf_(ent:solar_radiation, ent:unit, ent:w_m2).

% Temperature units
rdf_(ent:temperature, ent:units, ent:f).
rdf_(ent:f, ent:unit, 'f').
rdf_(ent:f, ent:description, 'degrees F').

% Percent units
rdf_(ent:dimensionless, ent:units, ent:percent).
rdf_(ent:percent, ent:unit, 'percent').
rdf_(ent:percent, ent:description, '%').

% Power units
rdf_(ent:power_density, ent:units, ent:w_m2).
rdf_(ent:w_m2, ent:unit, 'w/m^2').
rdf_(ent:w_m2, ent:description, 'watts/m^2').

rdf_(ent:operational, ent:description, 'operational conditions in open to which materiel may be subjected to during operations or standby').
rdf_(ent:operational, ent:name, 'operational').
rdf_(ent:operational, ent:link, ent:hourly_temperature).
rdf_(ent:operational, ent:link, ent:hourly_relative_humidity).
rdf_(ent:operational, ent:link, ent:groundTemperatureExtreme).
rdf_(ent:operational, ent:link, ent:solar).
rdf_(ent:operational, ent:link, ent:solar_radiation).
rdf_(ent:operational, ent:link, ent:elevation).

rdf_(ent:storageTransit, ent:description, 'conditions materiel may be subjected to in storage or transit').
rdf_(ent:storageTransit, ent:name, 'storage and transit').
rdf_(ent:storageTransit, ent:link, ent:storage_temperature).
rdf_(ent:storageTransit, ent:link, ent:storage_humidity).

rdf_(ent:ar7038_spec,  ent:groundReflectivity, 0.2).
rdf_(ent:ar7038_spec,  ent:nominalTime, 12). % noon
rdf_(ent:ar7038_spec,  ent:hours, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]). % noon
rdf_(ent:ar7038_spec,  ent:nominalElevation, 0).

/*
rdf_(ent:ar7038_spec,  ent:nominalDewpoint, ZZZZZZZ).
rdf_(ent:ar7038_spec,  ent:groundTemperatureExtreme,ZZZZZZZ).
rdf_(ent:ar7038_spec,  ent:nominalRelativeHumidity,ZZZZZZZ).
rdf_(ent:ar7038_spec,  ent:nominalBeta,ZZZZZZZ).
rdf_(ent:ar7038_spec,  ent:nominalGamma,ZZZZZZZ).
*/

% --------      hotDry
rdf_(ent:hotDry,  ent:hourly_temperature, [95.0, 94.0, 93.0, 92.0, 91.0, 90.0, 91.0, 95.0, 101.0,
                                   106.0, 110.0, 112.0, 116.0, 118.0, 119.0, 120.0, 119.0,
                                   118.0, 114.0, 108.0, 105.0, 102.0, 100.0, 98.0 ]).
rdf_(ent:hotDry,  ent:hourly_relative_humidity, [6.0, 7.0, 7.0, 8.0, 8.0, 8.0, 8.0, 6.0, 6.0, 5.0,
                                     4.0, 4.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 4.0,
                                     5.0, 6.0, 6.0, 6.0]).
rdf_(ent:hotDry,  ent:groundTemperatureExtreme, 146.0 ).
rdf_(ent:hotDry,  ent:wind, [13.0, 13.0] ).
rdf_(ent:hotDry,  ent:solar_radiation, [0,0,0,0,0,55,270,505,730,915,1040,1120,1120,1040,915,730,505,270,55,0,0,0,0,0]).
rdf_(ent:hotDry,  ent:solar, [39, -42.3284, -0.50676, 1.1291]).
rdf_(ent:hotDry,  ent:elevation, [3000.0, 15000.0, -5.0e-3, 4.0e-3] ).
rdf_(ent:hotDry,  ent:storage_temperature, [95.0, 94.0, 94.0, 92.0, 92.0, 91.0, 97.0, 104.0, 111.0,
                                   124.0, 133.0, 145.0, 156.0, 158.0, 160.0, 158.0, 153.0,
                                   145.0, 131.0, 118.0, 105.0, 103.0, 99.0, 95.0]).
rdf_(ent:hotDry,  ent:storage_humidity,  [6.0, 7.0, 7.0, 7.0, 7.0, 7.0, 5.0, 4.0, 4.0, 3.0,
                                     2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 3.0,
                                     5.0, 6.0, 6.0, 6.0]).
% --------   hotHumid

rdf_(ent:hotHumid,  ent:hourly_temperature, [88.0, 88.0, 88.0, 88.0, 88.0, 90.0, 93.0, 96.0, 98.0,
                                     100.0, 102.0, 104.0, 105.0, 105.0, 105.0, 105.0, 102.0,
                                     99.0, 97.0, 94.0, 91.0, 90.0, 89.0, 88.0 ]).
rdf_(ent:hotHumid,  ent:hourly_relative_humidity, [88.0, 88.0, 88.0, 88.0, 88.0, 85.0, 80.0, 76.0,
                                       73.0, 69.0, 65.0, 62.0, 59.0, 59.0, 59.0, 59.0,
                                       65.0, 69.0, 73.0, 79.0, 85.0, 85.0, 88.0, 88.0 ]).
rdf_(ent:hotHumid,  ent:groundTemperatureExtreme, 130.0 ).
rdf_(ent:hotHumid,  ent:wind, [8.0*m/s, 17.0*m/s] ).
rdf_(ent:hotHumid,  ent:solar_radiation, [0,0,0,0,0,45,315,560,790,950,1035,1080,1000,885,710,465,210,15,0,0,0,0,0,0]).
rdf_(ent:hotHumid,  ent:solar, [358, -7.6426, 0.199255, 0.981867] ).
rdf_(ent:hotHumid,  ent:elevation, [0,0,0,0] ).
rdf_(ent:hotHumid,  ent:storage_temperature, [95.0, 94.0, 94.0, 93.0, 92.0, 91.0, 97.0, 104.0, 111.0,
                                     124.0, 135.0, 144.0, 151.0, 156.0, 160.0, 156.0, 151.0,
                                     145.0, 136.0, 122.0, 105.0, 103.0, 99.0, 95.0]).
rdf_(ent:hotHumid,  ent:storage_humidity,  [67.0, 72.0, 75.0, 77.0, 79.0, 80.0, 70.0, 54.0,
                                       42.0, 31.0, 24.0, 17.0, 16.0, 15.0, 14.0, 16.0,
                                       18.0, 21.0, 29.0, 41.0, 53.0, 58.0, 62.0, 63.0]).

% --------       constantHighHumidity

rdf_(ent:constantHighHumidity,  ent:hourly_temperature, [75.0, 75.0, 75.0, 75.0, 75.0, 75.0, 75.0, 75.0, 75.0,
                                       75.0, 75.0, 75.0, 75.0, 75.0, 75.0, 75.0, 75.0, 75.0,
                                       75.0, 75.0, 75.0, 75.0, 75.0, 75.0 ]).
rdf_(ent:constantHighHumidity,  ent:hourly_relative_humidity, [100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 98.0, 97.0,
                                         95.0, 95.0, 95.0, 95.0, 95.0, 95.0, 95.0, 95.0, 95.0,
                                         95.0, 97.0, 98.0, 100.0, 100.0, 100.0, 100.0 ]).
rdf_(ent:constantHighHumidity,  ent:groundTemperatureExtreme, 130.0 ).
rdf_(ent:constantHighHumidity,  ent:wind, [8.0, 17.0] ).
rdf_(ent:constantHighHumidity,  ent:solar_radiation, Values) :- List range [1,24]/1, Values mapdot 0.0 ~> List.
rdf_(ent:constantHighHumidity,  ent:solar, [358, -7.6426, 0.199255, 0.981867] ).
rdf_(ent:constantHighHumidity,  ent:elevation, [0,0,0,0] ).
rdf_(ent:constantHighHumidity,  ent:storage_temperature, [80.0, 80.0, 80.0, 80.0, 80.0, 80.0, 80.0, 80.0, 80.0,
                                       80.0, 80.0, 80.0, 80.0, 80.0, 80.0, 80.0, 80.0, 80.0,
                                       80.0, 80.0, 80.0, 80.0, 80.0, 80.0]).
rdf_(ent:constantHighHumidity,  ent:storage_humidity,  List) :-
    rdf_(ent:hotHumid,  ent:storage_humidity,  List).

% --------          variableHighHumidity

rdf_(ent:variableHighHumidity,  ent:hourly_temperature, [80.0, 79.0, 79.0, 79.0, 78.0, 78.0, 81.0, 84.0,
                                          87.0, 89.0, 92.0, 94.0, 94.0, 95.0, 95.0, 93.0,
                                          92.0, 90.0, 88.0, 85.0, 83.0, 82.0, 81.0, 80.0 ]).
rdf_(ent:variableHighHumidity,  ent:hourly_relative_humidity, [100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 94.0, 88.0,
                                            82.0, 79.0, 77.0, 75.0, 74.0, 74.0, 74.0, 76.0, 79.0,
                                            82.0, 81.0, 91.0, 95.0, 96.0, 100.0, 100.0 ]).
rdf_(ent:variableHighHumidity,  ent:groundTemperatureExtreme, 130.0 ).
rdf_(ent:variableHighHumidity,  ent:wind, [0.0, 7.0] ).
rdf_(ent:variableHighHumidity,  ent:solar_radiation, [0,0,0,0,0,45,230,435,630,795,900,970,970,900,795,630,435,230,45,0,0,0,0,0]).
rdf_(ent:variableHighHumidity,  ent:solar, [303, -43.9901, -0.49382, 1.02379] ).
rdf_(ent:variableHighHumidity,  ent:elevation, [0,0,0,0] ).
rdf_(ent:variableHighHumidity,  ent:storage_temperature, [91.0, 90.0, 90.0, 88.0, 86.0, 88.0, 93.0, 101.0, 107.0,
                                          113.0, 124.0, 134.0, 142.0, 145.0, 145.0, 144.0, 140.0,
                                          134.0, 122.0, 111.0, 101.0, 95.0, 93.0, 91.0]).
rdf_(ent:variableHighHumidity,  ent:storage_humidity,  [69.0, 70.0, 71.0, 72.0, 74.0, 75.0, 64.0, 54.0,
                                             43.0, 36.0, 29.0, 22.0, 21.0, 20.0, 19.0, 20.0,
                                             21.0, 22.0, 32.0, 43.0, 54.0, 59.0, 63.0, 68.0]).

% --------           basicHot

rdf_(ent:basicHot,  ent:hourly_temperature, [91.0, 90.0, 90.0, 88.0, 86.0, 86.0, 88.0, 93.0, 99.0,
                                     102.0, 106.0, 107.0, 109.0, 110.0, 110.0, 110.0, 109.0,
                                     107.0, 104.0, 100.0, 97.0, 95.0, 93.0, 91.0 ]).
rdf_(ent:basicHot,  ent:hourly_relative_humidity, [36.0, 38.0, 41.0, 44.0, 44.0, 44.0, 41.0, 34.0, 29.0,
                                        24.0, 21.0, 18.0, 16.0, 15.0, 14.0, 14.0, 14.0, 15.0,
                                        17.0, 20.0, 22.0, 25.0, 28.0, 33.0 ]).
rdf_(ent:basicHot,  ent:groundTemperatureExtreme, 140.0 ).
rdf_(ent:basicHot,  ent:wind, [10.0, 16.0] ).
rdf_(ent:basicHot,  ent:solar, [39, -42.3284, -0.50676, 1.1291] ).
rdf_(ent:basicHot,  ent:solar_radiation, [0,0,0,0,0,55,270,505,730,915,1040,1120,1120,1040,915,730,505,270,55,0,0,0,0,0]).
rdf_(ent:basicHot,  ent:elevation, [3000.0, 10000.0, -5.0e-3, 4.0e-3] ).
rdf_(ent:basicHot,  ent:storage_temperature, [91.0, 90.0, 90.0, 88.0, 86.0, 88.0, 93.0, 101.0, 107.0,
                                     113.0, 124.0, 134.0, 142.0, 145.0, 145.0, 144.0, 140.0,
                                     134.0, 122.0, 111.0, 101.0, 95.0, 93.0, 91.0]).
rdf_(ent:basicHot,  ent:storage_humidity,  [36.0, 38.0, 41.0, 44.0, 44.0, 43.0, 37.0, 30.0, 23.0,
                                       17.0, 14.0, 8.0, 6.0, 6.0, 5.0, 6.0, 6.0, 6.0,
                                       10.0, 14.0, 19.0, 25.0, 28.0, 33.0]).

% --------          basicCold

rdf_(ent:basicCold,  ent:hourly_temperature, [-24.0, -25.0, -25.0, -25.0, -25.0, -25.0, -22.0, -18.0,
                                      -15.0, -12.0, -8.0, -5.0, -5.0, -6.0, -6.0, -8.0, -11.0,
                                      -13.0, -15.0, -17.0, -19.0, -21.0, -22.0, -24.0 ]).
rdf_(ent:basicCold,  ent:hourly_relative_humidity, [100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
                                        100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
                                        100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0]).
rdf_(ent:basicCold,  ent:groundTemperatureExtreme, -35.0 ).
rdf_(ent:basicCold,  ent:wind, [0.0, 16.0] ).
rdf_(ent:basicCold,  ent:solar_radiation, Values) :- List range [1,24]/1, Values mapdot 0.0 ~> List.
rdf_(ent:basicCold,  ent:solar, [0,0,0,0] ).
rdf_(ent:basicCold,  ent:elevation, [0,0,0,0] ).
rdf_(ent:basicCold,  ent:storage_temperature, [-27.0, -28.0, -28.0, -28.0, -28.0, -28.0, -27.0, -27.0,
                                      -26.0, -24.0, -22.0, -19.0, -17.0, -15.0, -13.0, -15.0,
                                      -18.0, -20.0, -22.0, -24.0, -26.0, -27.0, -27.0, -27.0]).
rdf_(ent:basicCold,  ent:storage_humidity,  Values) :- List range [1,24]/1, Values mapdot 0.0 ~> List.

% --------           cold

rdf_(ent:cold,  ent:hourly_temperature, [-50.0, -50.0, -50.0, -50.0, -50.0, -50.0, -49.0, -47.0, -45.0,
                                 -42.0, -39.0, -35.0, -35.0, -35.0, -35.0, -36.0, -38.0, -39.0,
                                 -41.0, -43.0, -45.0, -47.0, -48.0, -49.0 ]).
rdf_(ent:cold,  ent:hourly_relative_humidity, List) :-
    rdf_(ent:basicCold,  ent:hourly_relative_humidity, List).
rdf_(ent:cold,  ent:groundTemperatureExtreme, -50.0 ).
rdf_(ent:cold,  ent:wind, [0.0, 16.0] ).
rdf_(ent:cold,  ent:solar_radiation, Values) :- List range [1,24]/1, Values mapdot 0.0 ~> List.
rdf_(ent:cold,  ent:solar, [0,0,0,0] ).
rdf_(ent:cold,  ent:elevation, [0,0,0,0] ).
rdf_(ent:cold,  ent:storage_temperature, List) :-
    rdf_(ent:basicCold,  ent:storage_temperature, List).
rdf_(ent:cold,  ent:storage_humidity,  List) :-
    rdf_(ent:basicCold,  ent:storage_humidity, List).


% --------       severeCold

rdf_(ent:severeCold,  ent:hourly_temperature, [-60.0, -60.0, -60.0, -60.0, -60.0, -60.0, -60.0, -60.0,
                                       -60.0, -60.0, -60.0, -60.0, -60.0, -60.0, -60.0, -60.0,
                                       -60.0, -60.0, -60.0, -60.0, -60.0, -60.0, -60.0, -60.0 ]).
rdf_(ent:severeCold,  ent:hourly_relative_humidity, List) :-
    rdf_(ent:basicCold,  ent:hourly_relative_humidity, List).
rdf_(ent:severeCold,  ent:groundTemperatureExtreme, -50.0 ).
rdf_(ent:severeCold,  ent:wind, [0.0, 16.0] ).
rdf_(ent:severeCold,  ent:solar_radiation, Values) :- List range [1,24]/1, Values mapdot 0.0 ~> List.
rdf_(ent:severeCold,  ent:solar, [0,0,0,0] ).
rdf_(ent:severeCold,  ent:elevation, [0,0,0,0] ).
rdf_(ent:severeCold,  ent:storage_temperature, List) :-
    rdf_(ent:basicCold,  ent:storage_temperature, List).
rdf_(ent:severeCold,  ent:storage_humidity, List) :-
    rdf_(ent:basicCold,  ent:storage_humidity, List).



%                     z_min, z_max,    B  I_max, dIdZ
% int solar_dayOfYear,    double solar_latitude, double solar_LSToffset, double solar_clearnessNo


rdf_(ent:ar7038_environment,  ent:conditions_type, ent:highElevation).
rdf_(ent:ar7038_environment,  ent:conditions_type, ent:rain).
rdf_(ent:ar7038_environment,  ent:conditions_type, ent:snow).
rdf_(ent:ar7038_environment,  ent:conditions_type, ent:ice).
rdf_(ent:ar7038_environment,  ent:conditions_type, ent:hail).
rdf_(ent:ar7038_environment,  ent:conditions_type, ent:wind).
rdf_(ent:ar7038_environment,  ent:conditions_type, ent:sandDust).
rdf_(ent:ar7038_environment,  ent:conditions_type, ent:ozone).
rdf_(ent:ar7038_environment,  ent:conditions_type, ent:pressure).
rdf_(ent:ar7038_environment,  ent:conditions_type, ent:site_with_high_deteroration_rate).

rdf_(ent:highElevation, ent:elevation,   [10000.0*ft, 15000.0*ft, 20000.0*ft, 30000.0*ft, 40000.0*ft, 50000.0*ft]).
rdf_(ent:highElevation, ent:pressure,    [0.660*millibar, 0.520*millibar, 0.410*millibar, 0.255*millibar, 0.160*millibar, 0.100*millibar]).
rdf_(ent:highElevation, ent:temperature, [-42.0*f, -53.0*f, -68.0*f, -87.0*f, -98.0*f, -105.0*f]).


rdf_(ent:rain, env:characteristic, ent:dropletDistribution).
% connect to mapped distribution

/*
0.5, 1.4, 2662.
1.5, 2.4, 342.0
2.5, 3.4, 45.0
3.5, 4.4, 6.0
4.5, 5.4, 1.0
5.5, 6.4, 0.5
*/

rdf_(env:droplet_count_per_cubic_meter, ent:description, ' droplet count per cubic meter').

rdf_(ent:rain, ent:rain_rate_operational,  0.03).
rdf_(ent:rain_rate_operational, ent:description, 'instantaneous (1 min.) operational rainfall [in/min], 0.5% of hours during rainiest month').

rdf_(ent:rain, ent:rain_rate_eroding,  0.03).
rdf_(ent:rain_rate_eroding, ent:description, 'max. rainfall [in/min] for use w/eroding materiel, 0.1% of hours during rainiest month').


rdf_(ent:snow, ent:snow_density,  6.24*lb/ft^3).
rdf_(ent:snow_density, ent:description, 'approx. density of snow [lbm/cuft]').

rdf_(ent:snow, ent:snow_rate,  3.17*in/hr).
rdf_(ent:snow_rate, ent:description, 'accumulation rate [in/h] over 24 h period').

rdf_(ent:snow, ent:snow_load_portableEquipment,  10.0*lb/ft^2).
rdf_(ent:snow_load_portableEquipment, ent:description, 'load [lbf/sqft] imposed by accumulated snow on portable equipment such as tents').

rdf_(ent:snow, ent:snow_load_temporaryEquipment,  20.0*lb/ft^2).
rdf_(ent:snow_load_temporaryEquipment, ent:description, 'load [lbf/sqft] imposed by accumulated snow on large items such as rigid shelters, portable hangars, etc.').

rdf_(ent:snow, ent:snow_load_semipermanentEquipment,  48.0*lb/ft^2).
rdf_(ent:snow_load_semipermanentEquipment, ent:description, 'load [lbf/sqft] imposed by accumulated snow on semi-permanent equipment').

rdf_(ent:snow, ent:massflux_blowingSnow,  [[ 0.16, 0.33, 0.82, 1.6, 2.5, 3.3, 8.2, 16.0, 25.0, 33.0],
                                           [0.109, 0.041, 0.014, 0.0066, 0.0045, 0.0033, 0.0014, 0.00082, 0.00068, 0.00045]]).

rdf_(ent:massflux_blowingSnow, ent:description, 'horizontal massflux [lbm/sqft-s] of blowing snow').

%  ice
rdf_(ent:ice, ent:glaze_ice_density,  56.2*lb/ft^3).
rdf_(ent:glaze_ice_density, ent:description, 'approx. glaze ice density [lbm/cuft]').

rdf_(ent:ice, ent:glaze_ice_operational,  0.5*in).
rdf_(ent:glaze_ice_operational, ent:description, 'glaze ice thickness [in], operational design thickness for all weather operation').

rdf_(ent:ice, ent:glaze_ice_noPermanentDamage,  3.0*in).
rdf_(ent:glaze_ice_noPermanentDamage, ent:description, 'glaze ice thickness [in], operational design thickness for all weather operation').

%  ice/rime
rdf_(ent:ice, ent:glaze_rime_mix_density,  16.2*lb/ft^3).
rdf_(ent:glaze_rime_mix_density, ent:description, 'approx. glaze/rime mixture density [lbm/cuft]').

rdf_(ent:ice, ent:glaze_rime_mix_noPermanentDamage,  6.0*in).
rdf_(ent:glaze_rime_mix_noPermanentDamage, ent:description, 'glaze/rime ice thickness [in], failure acceptable DURING icing but no permanent damage ').

% rime
rdf_(ent:ice, ent:note, 'hoarfrost (only type of ice accretion that occurs when T << 32F)').

rdf_(ent:ice, ent:rime_density,  12.5*lb/ft^3).
rdf_(ent:rime_density, ent:description, 'approx. rime mixture density [lbm/cuft]').

rdf_(ent:ice, ent:glaze_rime_mix_noPermanentDamage,  x*in).
/*
  if (height_ft <= 0.0) return 6.0;
  if (height_ft >= 400.0) return 20.0;
  return 6.0 + 0.035*height_ft;

*/
rdf_(ent:rime_noPermanentDamage, ent:description, 'rime ice thickness [in], failure acceptable DURING icing but no permanent damage ').

% hoarfrost
rdf_(ent:ice, ent:hoarfrost_density_max,  12.5*lb/ft^3).
rdf_(ent:hoarfrost_density_max, ent:description, 'upper bound on hoarfrost density [lbm/cuft]').

rdf_(ent:ice, ent:hoarfrost_max,  7.0*in).
rdf_(ent:hoarfrost_max, ent:description, 'upper-bound on hoarfrost thickness [in]').

% hail
rdf_(ent:hail, ent:diameter, 2.0*in).
rdf_(ent:hail, ent:description, 'hail diameter [in], design value').

% wind
rdf_(ent:wind, ent:steadyWindVelocity_10ft, 73.0*ft/s).
rdf_(ent:steadyWindVelocity_10ft, ent:description, 'steady wind velocity [ft/s], operational extreme at 10 ft').
rdf_(ent:wind, ent:gustWindVelocity_10ft, 95.0*ft/s).
rdf_(ent:gustWindVelocity_10ft, ent:description, 'gust velocity [ft/s], operational extreme').
% need speeds away from 10ft, interpolated

% sand dust
rdf_(ent:sandDust, ent:description, 'most airborne particles are < diameter_max_typical').
rdf_(ent:sandDust, ent:diameter_min, 3.94e-6*in).
rdf_(ent:sandDust, ent:diameter_max, 3.94e-2*in).
rdf_(ent:sandDust, ent:diameter_max_typical, 2.91e-3*in).

rdf_(ent:sandDust, ent:sandDustEnvironment, ent:aircraftVicinityConditions).
rdf_(ent:sandDust, ent:sandDustEnvironment, ent:surfaceVehicleVicinityConditions).
rdf_(ent:sandDust, ent:sandDustEnvironment, ent:naturalVicinityConditions).

rdf_(ent:aircraftVicinityConditions, ent:particle_density, 1.32e-4*lb/ft^3).
rdf_(ent:aircraftVicinityConditions, ent:particle_diameter_max, 1.97e-2*in).
rdf_(ent:aircraftVicinityConditions, ent:extinction_coefficient, 100/km).
rdf_(ent:extinction_coefficient, ent:description, 'extinction coefficient [/km] for visible light to 12 micron (IR)').

rdf_(ent:surfaceVehicleVicinityConditions, ent:particle_density, 6.61e-5*lb/ft^3).
rdf_(ent:surfaceVehicleVicinityConditions, ent:particle_diameter_min, 2.91e-3*in).
rdf_(ent:surfaceVehicleVicinityConditions, ent:particle_diameter_max, 3.94e-2*in).
rdf_(ent:surfaceVehicleVicinityConditions, ent:particle_diameter_min_typical, 2.91e-3*in).
rdf_(ent:surfaceVehicleVicinityConditions, ent:particle_diameter_max_typical, 13.8e-3*in).
rdf_(ent:surfaceVehicleVicinityConditions, ent:vehicle_wind_max, 59.0*ft/s).
rdf_(ent:vehicle_wind_max, ent:description, 'max. wind speed [ft/s] at 10 ft').

rdf_(ent:naturalConditions, ent:particle_density, 1.10e-5*lb/ft^3).
rdf_(ent:naturalConditions, ent:particle_diameter_max_typical, 5.9e-3*in).
rdf_(ent:naturalConditions, ent:particle_diameter_max, 3.94e-2*in).
rdf_(ent:naturalConditions, ent:particle_diameter_min_typical, 2.91e-3*in).
rdf_(ent:naturalConditions, ent:fine_particle_diameter_max_typical, 5.9e-3*in).
rdf_(ent:naturalConditions, ent:vehicle_wind_max, 59.0*ft/s).
rdf_(ent:naturalConditions, ent:temperature_min_typical, 70.0*f).
rdf_(ent:naturalConditions, ent:relativeHumidity_max_typical, 70.0*f).
rdf_(ent:naturalConditions, ent:largee_particle_diameter_max, 3.94e-2*in).
rdf_(ent:naturalConditions, ent:largee_particle_diameter_min_typical, 5.9e-3*in).
rdf_(ent:naturalConditions, ent:largee_particle_diameter_max_typical, 1.972e-2*in).

rdf_(ent:ozone, ent:density, 1.37e-8*lb/ft^3).

rdf_(ent:pressure, ent:max_pressure, 1.080*bar). % is that   lb/in^2
rdf_(ent:pressure, ent:min_pressure, 0.508*bar).
rdf_(ent:pressure, ent:sea_level_pressure, 0.877*bar).


rdf_(ent:site_with_high_deteroration_rate, ent:description, 'table-2.5, Type Material	Type of Site with the Highest Deterioration Rates').
rdf_(ent:elastomers, ent:site_with_high_deteroration_rate, ['open']).
rdf_(ent:polymers, ent:site_with_high_deteroration_rate, ['open']).
rdf_(ent:textiles, ent:site_with_high_deteroration_rate, ['forest']).
rdf_(ent:metals, ent:site_with_high_deteroration_rate, ['coastal swamp', 'mangrove', 'forst']).

