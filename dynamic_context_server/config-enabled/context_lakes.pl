:- module(context_lakes, [lat_list/6,
                         get_all_records/1,
                         get_all_records/2]).

/** <module> Context model lake data gathering
    * Lake sizes
    * Ice-Out example
*/

:- context:register(context_lakes:navigate).
:- context:register(context_lakes:plot_chart).
:- context:register(context_lakes:set_yearly_range).

:- use_module(components(messages)).

%%   navigate(+Request)
%
%    Dynamic page to lake models
navigate(Request) :-
    reply_html_page(
        cliopatria(default),
        [title('Lake Home')],
        [
         \(con_text:table_with_iframe_target(
               Request,
               [
                h2('Lake query examples'),
		                \(con_text:table_form('Clear and load data ', 'set_yearly_range',
				      [[from,1843],
				       [to, 2012]])),

                \(con_text:table_form_target(
                                         'What latitude to plot? >> ',
                                         'plot_chart',
                                         target_iframe,
                                         [['latitude', '47.0']]
                                                      )
		 ),
		i([ br([]),
		    'or click\n on map ',
		    \(context_map:minnesota_map('plot_chart?latitude=~w&longitude=~w',target_iframe))
		  ]
		 )
               ]
                                   )
          )
        ]
                   ).

%%   plot_chart(+Request)
%
%    Plot ice-out data over a historical range of interest
plot_chart(Request) :-
    http_parameters(Request, [latitude(Latitude, [float, default(46.0)])]),
    Lat is integer(Latitude),
    FN = '/html/images/xy.bmp',
    lat_list(FN, Lat, _Years, _Times, N, Slope),
    reply_html_page(title('Ice-Out Chart'),
                   [ p(['Fitted slope = ', Slope, ' days/year to ',
                       N, ' data points.']),
                     img(src(FN))
                   ]).

%%   set_yearly_range(+Request)
%
%    Set yearly range for collecting data
set_yearly_range(Request) :-
    http_parameters(Request, [from(From, [integer, default(1843)]),
                              to(To, [integer, default(2012)])]),

   call_showing_messages(
       (
       print_message(informational, format('Years ~d to ~d', [From, To])),
       get_all_records(From, To),
       reply_html_page(title('Configuration'),
                   [
                     p(['Configured from ', From, ' to ', To]),
                     \(con_text:back_up(1))
                   ])
       ),
       []
                        ).



%%   minnesota_dnr_ice_out(-URL)
%
%    Location of ice-out data
minnesota_dnr_ice_out('http://www.dnr.state.mn.us/services/climatology/ice_out_by_year.html?year=').

:- dynamic
    temperature/6.

%%   assert_temperature(+Name, +Lat, +Year, +Month, +Date, +Days)
%
%    Not really a temperature but asserting the date of ice-out
assert_temperature(Name, Lat, Year, Month, Date, Days) :-
    not(temperature(Name, Lat, Year, Month, Date, Days)),
    print_message(informational, format('Lake ~w @ ~w N (~w/~w/~w [day ~w]))',
                                         [Name, Lat, Month, Date,  Year, Days])),
    asserta(temperature(Name, Lat, Year, Month, Date, Days)),
    !.
assert_temperature(_,_,_,_,_,_).

%%   store_record(+Term)
%
%    Store record
store_record(Term) :-
    Term=json(
             [ice_out_first_year=_IceOutFirstYear,
              ice_out_last_year=_IceOutLastYear,
              lat=Lat,
              name=Name,
              ice_out_earliest=_IceOutEarliest,
              ice_out_latest=_IceOutLatest,
              ice_out_date=IceOutDate,
              sentinel_lake=_SentinelLake,
              ice_out_number_of_entries=_IceOutNumberOfEntries,
              id=_Id,
              lon=_Lon,
              ice_out_median_since_1950=_IceOutMedianSince1950]
             ),
    atomic_list_concat([Year,Month,Date], '-', IceOutDate),
    parse_time(IceOutDate, Stamp),
    atom_concat(Year, '-01-01', YearStart),
    parse_time(YearStart, Stamp0),
    Days is (Stamp-Stamp0)/24/60/60,
    % print(user_error, [Name, Lat, Year, Month, Date, Days, '\n']),
    assert_temperature(Name, Lat, Year, Month, Date, Days).


%%   get_ice_out(-Year)
%
%    Retrieve data from remote server for given year and save
get_ice_out(Year) :-
    minnesota_dnr_ice_out(URL),
    atom_concat(URL, Year, U),
    http_client:http_get(U, R, []),
    atom_json_term(R, J, []),
    J=json([status='OK', results=L, message='']),
    maplist(store_record,L).

%%   remove_temperatures
%
%    Clear out ice-out data
remove_temperatures :-
    retractall(temperature(_,_,_,_,_,_)).

%%   temperature_lat(+Lat_Range,-Year,-Time)
%
%    Query to data at a given latitude
temperature_lat(Lat_Range,Year,Time) :-
    temperature(_Name,Lat,Y,_Month,_Day,Time),
    atom_number(Lat, Lat_N),
    L is floor(Lat_N),
    Lat_Range = L,
    atom_number(Y, Year).

%%   lat_list(+FN, +Lat, -Years, -Times, -N, -Slope)
%
%    Plot all records at a given latitude
lat_list(FN, Lat, Years, Times, N, Slope) :-
    findall(Y, temperature_lat(Lat,Y,T), Years),
    findall(T, temperature_lat(Lat,Y,T), Times),
    format(atom(Title), '"Minnesota Latitude : ~D N"', [Lat]),
    context_r:rplot_with_regression(FN, Years, Times, Title, '"year"', '"iceOutDay"', Slope),
    length(Years,N).

%%   get_all_records(+Year)
%
%    get all records up to year
get_all_records(1843).
get_all_records(To_Year) :-
    get_ice_out(To_Year),
    Next is To_Year - 1,
    get_all_records(Next).

%%   get_all_records(+From, +To)
%
%    get all records up to year
get_all_records(From, To) :-
    remove_temperatures, !,
    findall(Year, between(From, To, Year), List),
    maplist(get_ice_out, List).


