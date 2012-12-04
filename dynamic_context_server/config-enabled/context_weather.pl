:- module(context_weather, []).

/** <module> Context weather interface
    * Locale specific seasonal generic
    * Locale specific diurnal
*/


:- context:register(context_weather:navigate).
:- context:register(context_weather:measure).

:- use_module(context_math).
:- use_module(library(semweb/sparql_client)).
:- use_module(context_dbpedia).

months(     [jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec]).
middle_days([ 15, 45, 75,105,135,165,195,225,255,285,315,350]).
month_number([ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12]).


get_month(Day, Month) :-
     middle_days(Days),
     months(Months),
     nth0(MonthNum, Days, Day),
     nth0(MonthNum, Months, Month).

measure_spread(Month, Measure, Q) :-
   format(atom(Q),'dbpprop:~w~w ?~w~w; ', [Month, Measure, Month, Measure]).

format_measure_query(Text, Measure) :-
    months(Months),
    findall(Each, (member(Month,Months),
                   measure_spread(Month,Measure,Each)), Text).

get_measure_spread(Location, Measure, Measures, Name) :-
    context_dbpedia:get_uri(Location, URI),
    print(user_error, ['======',URI, '======']),
    format_measure_query(Text, Measure),
    atomic_list_concat(Text, Temps),
    Format = 'select * where {<~w> ~w dbpprop:location ?L.}',
    format(atom(Q), Format, [URI, Temps]),
    sparql_query( Q,  Row, [ host('dbpedia.org'), path('/sparql/')] ),
    Row = row(H1,H2,H3,H4, H5, H6, H7, H8,H9,H10,H11,H12,
              literal(lang(en, Name))),
    context:strip_numbers([H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12], [], Measures).

find_min([], [], Final, Final, Day, Day).
find_min([Num | Rest],
         [Day | Rest_of_Days],
         Lowest, Final, Index, Final_Day) :-
    (   Num < Lowest ->
        find_min(Rest, Rest_of_Days, Num, Final, Day, Final_Day)
    ;
        find_min(Rest, Rest_of_Days, Lowest, Final, Index, Final_Day)
    ).
find_min_index(List, Min, Day) :-
    middle_days(Days),
    find_min(List, Days, 1000, Min, 0, Day), !.


find_max([], [], Final, Final, Day, Day).
find_max([Num | Rest],
         [Day | Rest_of_Days],
         Highest, Final, Index, Final_Day) :-
    (   Num > Highest ->
        find_max(Rest, Rest_of_Days, Num, Final, Day, Final_Day)
    ;
        find_max(Rest, Rest_of_Days, Highest, Final, Index, Final_Day)
    ).
find_max_index(List, Max, Day) :-
    middle_days(Days),
    find_max(List, Days, -1000, Max, 0, Day), !.

process_location(Location,
		 Measure,
                 Min,
		 MinDay,
                 Max,
		 MaxDay,
                 Values,
                 Name) :-
    get_measure_spread(Location, Measure, Values,Name), !,
    find_min_index(Values, Min, MinDay),
    find_max_index(Values, Max, MaxDay).


get_all_weather_sets(List) :-
    findall(option([value(ID)],[Name]), available_location(ID, Name), L),
    sort(L, List).

available_location(Locale, Title) :-
     rdfS(Locale, ent:title, Title),
     rdf(URI, ent:locale, Locale),
     rdfS(URI, ent:feature, 'thermal features').
available_location(Locale, Title) :-
     rdfS(Locale, ent:title, Title),
     rdfS(Locale, ent:feature, 'thermal features').

units('SnowInch', 'Snowfall', in).
units('RainDays', 'Rain days', days).
units('PrecipitationInch', 'Precipitation', in).
units('Sun', 'Solar insolation', 'w/m^2').


navigate -->
    {
     get_all_weather_sets(List)
    },
    html(
       form([action('/context_weather/measure'),
             target('target_iframe')],
            [
             select([name('site')], List),
	     br([]),
	     \(con_text:radio_toggles(
			    'measure',
			    [['Monthly Rain Precip (in)', 'PrecipitationInch'],
                             ['Monthly Snow Amount (in)', 'SnowInch'],
			     ['Monthly Rain Days', 'RainDays'],
			     ['Monthly average Solar (W/m^2)', 'Sun']
			    ])),
             input([type('submit'), value('Select Data Set')])
            ])
        ).

measure(Request) :-
   http_parameters(Request, [site(Site, [string]),
			     measure(Measure, [string])]),
   process_location(Site,  Measure, Min, MinDay, Max, MaxDay, Values, Name),
   context_dbpedia:get_lat_lon(Site,Lat, Lon),
   context_dbpedia:get_depiction(Site, Image),
   context_dbpedia:get_page(Site, Page),
   middle_days(Days),
   get_month(MinDay, MinMonth),
   get_month(MaxDay, MaxMonth),
   units(Measure, Title, Units),
   Profile tuple Days + Values,
   reply_html_page([title(Site),
                     \(con_text:style)
                    ],
                    [h2([Name, ' @ Lat/Lon : ', Lat, '/', Lon]),
                     div([align=center],
                         [
                          \(con_text:table_multiple_entries([['Extreme', 'Min', 'Max']],
                                                            [[Measure, Min,   Max],
                                                             [i('month'),i(MinMonth), i(MaxMonth)]])),
                          br([])
                         ]),
                     img([height=300, align=right, class=opaque, src=Image]),  % class=opaque
                     \(context_graphing:dygraph_native(lin,
                                                    ['Day', Measure],
                                                    'day',
                                                    Units,
                                                    ['Average ', Title],
                                                    Profile)),
                     br([]),
                     \(con_text:inline_button(
                                   \(con_text:button_link('Display Map',
					   '/context_map/view',
					   render,
					   [[lat, Lat],
					    [lon, Lon],
					    [title, Site]
					   ]))
                                            )
                     ),
                     \(con_text:inline_button(
                                    \(con_text:button_link('Display Info Page', Page, render))
                                             )
                     )
                    ]).

measure(Request) :-
   http_parameters(Request, [site(Site, [string]),
			     measure(Measure, [string])]),
   reply_html_page([title(Site),
                     \(con_text:style)
                    ],
                    [h2([Site, ' measure ', Measure, ' was not found'])
                   ]).
