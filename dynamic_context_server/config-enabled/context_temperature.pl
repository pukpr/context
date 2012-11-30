:- module(context_temperature, [
                                calculate_numeric_temperature/3
                               ]).

/** <module> Context temperature interface
    * Locale specific seasonal
    * Locale specific diurnal
*/


:- context:register(context_temperature:temperature).
:- context:register(context_temperature:navigate).
:- use_module(context_math).
:- use_module(library(semweb/sparql_client)).

% T(t) = T0+Ty*sin(2*pi/365*t+a)+(dT*sin(2*pi/365*t+b)+(Td-dT))*sin(2*pi*t+c)


calculate_symbolic_temperature(Site, T, S) :-
    rdfS(U, ent:name, Site),
    rdfR(U, ent:t0, T0),
    rdfR(U, ent:ty, Ty),
    rdfR(U, ent:dT, DT),
    rdfR(U, ent:td, Td),
    rdfR(U, ent:a, A),
    rdfR(U, ent:b, B),
    rdfR(U, ent:c, C),
    PI is pi,
    S = T0+Ty*sin(2*PI/365*T+A)+(DT*sin(2*PI/365*T+B)+(Td-DT))*sin(2*PI*T+C).

calculate_numeric_temperature(Site, T, Temperature) :-
    calculate_symbolic_temperature(Site, T, Symbolic_Temperature),
    Temperature is Symbolic_Temperature.

generate_yearly_temperature_profile(Site, Timespan, Profile) :-
   Timespan range [0,365]/0.1,
   Profile mapdot calculate_numeric_temperature(Site) ~> Timespan.

begin_c_skeleton('
#include <math.h>

float Get_Temperature (float time) {
   return ('
).

end_c_skeleton(');
}'
).

generate_c_code(Site, Code) :-
   calculate_symbolic_temperature(Site,time,ST),
   begin_c_skeleton(Inner),
   end_c_skeleton(Outer),
   context_codegen:generate_from_symbolic_skeleton(Inner, ST, Outer, Code).

temperature(Request) :-
   http_parameters(Request, [site(Site, [string])]),
   ( generate_yearly_temperature_profile(Site, TS, Profile)
   ->
     generate_c_code(Site, Code),
     reply_html_page(% cliopatria(default),
                     [title(Site),
                     %\(context_graphing:dygraph_script_load)
                     \(con_text:style)
                    ],
                    [h2('Interactive graph'),
                     \(context_graphing:dygraph_plot(false,
                                                    'Time, Temperature',
                                                    'day',
                                                    'temperature(C)',
                                                    ['Expected ', Site, ' temperature'],
                                                    [TS, Profile])),
                    br([]),
                    h3('The generated c-code:'),
                    pre(Code)
                   ])
   ;
     process_location(Site,
                      cold(MinCold, MaxCold, DayCold),
                      hot(MinHot, MaxHot, DayHot),
                      Lows,
                      Highs,
                      Name)
   ->
     get_lat_lon(Site,Lat, Lon),
     middle_days(Days),
     get_month(DayCold, ColdMonth),
     get_month(DayHot, HotMonth),
     Dual_Profile tuple Days + Lows + Highs,
     reply_html_page([title(Site),
                     \(con_text:style)
                    ],
                    [h2([Name, ' @ Lat/Lon : ', Lat, '/', Lon]),
                     div([align=center],
                         [
                          \(con_text:table_multiple_entries([['MinCold', 'MaxCold', 'MinHot', 'MaxHot']],
                                                            [[MinCold,   MaxCold,   MinHot,   MaxHot],
                                                             [i(ColdMonth), i(ColdMonth), i(HotMonth), i(HotMonth)]])),
                          br([])
                         ]),
                     \(context_graphing:dygraph_native(lin,
                                                    ['Day', 'Lows', 'Highs'],
                                                    'day',
                                                    'Temperature(F)',
                                                    ['Average temperature (degrees F)'],
                                                    Dual_Profile)),
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
                     )
                    ])
   ;
     reply_html_page([title(Site),
                     \(con_text:style)
                    ],
                    [h2([Site, ' was not found'])
                   ])
   ).

example -->
   html(
       \(con_text:table_form_target('Temperature profile >> ',
				    '/context_temperature/temperature',
				    target_iframe,
				    [['site', 'Wilmington']]
				   )
		     )
		   ).

% -----

navigate -->
    {
     get_all_temperature_sets(List)
    },
    html(
       form([action('/context_temperature/temperature'),
             target('target_iframe')],
            [
             select([name('site')], List),
             input([type('submit'), value('Select Data Set')])
            ])
        ).

get_all_temperature_sets(List) :-
    findall(option([value(ID)],[Name]), available_location(ID, Name), L),
    sort(L, List).

available_location(Locale, Title) :-
     rdfS(Locale, ent:title, Title),
     rdf(URI, ent:locale, Locale),
     rdfS(URI, ent:feature, 'thermal features').
available_location(Locale, Title) :-
     rdfS(Locale, ent:title, Title),
     rdfS(Locale, ent:feature, 'thermal features').

% ------------- dbpedia interface

months(     [jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec]).
middle_days([ 15, 45, 75,105,135,165,195,225,255,285,315,350]).
month_number([ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12]).


get_month(Day, Month) :-
     middle_days(Days),
     months(Months),
     nth0(MonthNum, Days, Day),
     nth0(MonthNum, Months, Month).

temperature_spread(Month, Q) :-
   format(atom(Q),
          'dbpprop:~wHighF ?~wHighF; dbpprop:~wLowF ?~wLowF; ',
          [Month, Month, Month, Month]).

format_temperature_query(Text) :-
    months(Months),
    findall(Each, (member(Month,Months),
                   temperature_spread(Month,Each)), Text).

strip_numbers([], Input, Final) :- reverse(Input, Final).
strip_numbers([literal(type(_, Str))| Rest], Input, Final) :-
    atom_number(Str, Num),
    strip_numbers(Rest, [Num|Input], Final).

get_lat_lon(Location, Lat, Lon) :-
    (
       rdf_global_id(dbpedia:_, Location) ->
       Local = Location
    ;
       rdf_global_id(dbpedia:Location, Local)
    ),
/*
    (
       rdf_global_id(dbpedia:Local, Location) ->
       true
    ;
       Local = Location
    ),
    Format = 'select * where {dbpedia:~w geo:lat ?Lat; geo:long ?Long.}',
*/
    Format = 'select * where {<~w> geo:lat ?Lat; geo:long ?Long.}',
    format(atom(Q), Format, [Local]),
    sparql_query(
         Q,
         row(C1, C2),
         [ host('dbpedia.org'), path('/sparql/')]
                 ),
    strip_numbers([C1, C2], [], [Lat, Lon]), !.
get_lat_lon(_Location, '?', '?').

get_temperature_spread(Location, Lows, Highs, Name) :-
    (
       rdf_global_id(dbpedia:_, Location) ->
       Local = Location
    ;
       rdf_global_id(dbpedia:Location, Local)
    ),
    print(user_error, ['======',Local, '======']),
    format_temperature_query(Text),
    atomic_list_concat(Text, Temps),
    % Format = 'select * where {dbpedia:~w ~w dbpprop:location ?L.}',
    Format = 'select * where {<~w> ~w dbpprop:location ?L.}',
    % Format = 'select * where {dbpedia:~w ~w rdfs:label ?L.}',
    format(atom(Q), Format, [Local, Temps]),
    sparql_query(
         Q,
         Row,
         [ host('dbpedia.org'), path('/sparql/')]
                 ),
    Row = row(H1,L1,H2,L2,H3,L3,H4, L4, H5, L5, H6, L6,
              H7,L7,H8,L8,H9,L9,H10,L10,H11,L11,H12,L12,
              literal(lang(en, Name))),
    strip_numbers([L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12], [], Lows),
    strip_numbers([H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12], [], Highs).

find_min([], [], Final, Final, Day, Day).
find_min([Num | Rest],
         [Day | Rest_of_Days],
         Lowest, Final, Index, Final_Day) :-
    (   Num < Lowest ->
        find_min(Rest, Rest_of_Days, Num, Final, Day, Final_Day)
    ;
        find_min(Rest, Rest_of_Days, Lowest, Final, Index, Final_Day)
    ).
find_min_index(List, Highs, Min, Day, Max) :-
    middle_days(Days),
    find_min(List, Days, 1000, Min, 0, Day),
    nth0(N,Days,Day),
    nth0(N,Highs, Max), !.


find_max([], [], Final, Final, Day, Day).
find_max([Num | Rest],
         [Day | Rest_of_Days],
         Highest, Final, Index, Final_Day) :-
    (   Num > Highest ->
        find_max(Rest, Rest_of_Days, Num, Final, Day, Final_Day)
    ;
        find_max(Rest, Rest_of_Days, Highest, Final, Index, Final_Day)
    ).
find_max_index(List, Lows, Max, Day, Min) :-
    middle_days(Days),
    find_max(List, Days, -1000, Max, 0, Day),
    nth0(N,Days,Day),
    nth0(N,Lows, Min), !.

process_location(Location,
                 cold(MinCold, MaxCold, DayCold),
                 hot(MinHot, MaxHot, DayHot),
                 Lows,
                 Highs,
                 Name) :-
    get_temperature_spread(Location, Lows, Highs, Name), !,
    find_min_index(Lows, Highs, MinCold, DayCold, MaxCold),
    find_max_index(Highs, Lows, MaxHot, DayHot, MinHot).


/*
PREFIX dbpedia-owl:	<http://dbpedia.org/ontology/>
PREFIX dbpedia:	<http://dbpedia.org/resource/>
PREFIX dbpprop:	<http://dbpedia.org/property/>

select distinct ?Concept where { dbpedia:Baltimore dbpprop:augHighF ?Concept
} LIMIT 100
*/


/*
connect_for_data(Location) :-
     Format =
'select * where {
  dbpedia:~w
    dbpprop:janLowF ?JanH ;
    dbpprop:augHighF ?AugT ;
    dbpprop:junHighF ?JunT .
                }',

     format(atom(Q), Format, Location),
     sparql_query(
         Q,
         Row,
         [ host('dbpedia.org'), path('/sparql/')]
                 ),
     print(Row).
*/

/*
mydata(
    _,
    row(literal(type(xsd:int, '44')), literal(type(xsd:int, '29')), literal(type(xsd:int, '47')), literal(type(xsd:int, '31')), literal(type(xsd:int, '57')), literal(type(xsd:int, '39')), literal(type(xsd:int, '68')), literal(type(xsd:int, '48')), literal(type(xsd:int, '77')), literal(type(xsd:int, '58')), literal(type(xsd:int, '86')), literal(type(xsd:int, '68')), literal(type(xsd:int, '91')), literal(type(xsd:int, '73')), literal(type(xsd:int, '88')), literal(type(xsd:int, '71')), literal(type(xsd:int, '81')), literal(type(xsd:int, '64')), literal(type(xsd:int, '70')), literal(type(xsd:int, '52')), literal(type(xsd:int, '59')), literal(type(xsd:int, '42')), literal(type(xsd:int, '49')), literal(type(xsd:int, '33')), literal(type(xsd:int, '1776')))
    ).
*/

/*
test_dbpedia :-
    context_temperature:process_location('Baltimore',
                                         cold(MinCold, MaxCold, DayCold),
                                         hot(MinHot, MaxHot, DayHot),
                                         Name).

*/


