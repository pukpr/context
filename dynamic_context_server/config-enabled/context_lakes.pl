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
    FN = '/html/images/xy.svg',
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
% minnesota_dnr_ice_out('http://www.dnr.state.mn.us/services/climatology/ice_out_by_year.html?year=').
minnesota_dnr_ice_out('https://maps.dnr.state.mn.us/cgi-bin/climatology/ice_out_by_year.cgi?year=').

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

%%   default_max_date(-Date)
%
%    Default maximum date for sorting when date is missing
default_max_date('9999-99-99').

%%   store_record(+Term)
%
%    Store record - now handles fields in any order
store_record(json(Fields)) :-
    memberchk(lat=Lat, Fields),
    memberchk(name=Name, Fields),
    memberchk(ice_out_date=IceOutDate, Fields),
    atomic_list_concat([Year,Month,Date], '-', IceOutDate),
    parse_time(IceOutDate, Stamp),
    atom_concat(Year, '-01-01', YearStart),
    parse_time(YearStart, Stamp0),
    Days is (Stamp-Stamp0)/24/60/60,
    % print(user_error, [Name, Lat, Year, Month, Date, Days, '\n']),
    assert_temperature(Name, Lat, Year, Month, Date, Days).

%%   record_key(+Record, -Key)
%
%    Extract a unique key from a record for deduplication
record_key(json(Fields), Key) :-
    (memberchk(id=Id, Fields) -> true ; Id = ''),
    (memberchk(name=Name, Fields) -> true ; Name = ''),
    (memberchk(ice_out_date=Date, Fields) -> true ; Date = ''),
    Key = key(Id, Name, Date).

%%   remove_duplicate_records(+Records, -UniqueRecords)
%
%    Remove duplicate records based on id, name, and ice_out_date
%    Uses a list of seen keys for O(n) average performance
remove_duplicate_records(Records, Unique) :-
    remove_duplicates_with_set(Records, [], [], Unique).

%%   remove_duplicates_with_set(+Records, +SeenKeys, +Acc, -Unique)
%
%    Helper that tracks seen keys for efficient deduplication
remove_duplicates_with_set([], _SeenKeys, Acc, Unique) :-
    reverse(Acc, Unique).
remove_duplicates_with_set([Record|Rest], SeenKeys, Acc, Unique) :-
    record_key(Record, Key),
    (memberchk(Key, SeenKeys) ->
        % Duplicate found, skip it
        remove_duplicates_with_set(Rest, SeenKeys, Acc, Unique)
    ;
        % Not a duplicate, add key to seen set and keep record
        remove_duplicates_with_set(Rest, [Key|SeenKeys], [Record|Acc], Unique)
    ).

%%   sort_records(+Records, -SortedRecords)
%
%    Sort records by ice_out_date, then name, then id
sort_records(Records, Sorted) :-
    map_list_to_pairs(record_sort_key, Records, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

%%   record_sort_key(+Record, -SortKey)
%
%    Extract sort key from record
record_sort_key(json(Fields), Key) :-
    default_max_date(DefaultDate),
    (memberchk(ice_out_date=Date, Fields) -> true ; Date = DefaultDate),
    (memberchk(name=Name, Fields) -> true ; Name = ''),
    (memberchk(id=Id, Fields) -> true ; Id = ''),
    Key = sort_key(Date, Name, Id).


%%   get_ice_out(-Year)
%
%    Retrieve data from remote server for given year and save
get_ice_out(Year) :-
    minnesota_dnr_ice_out(URL),
    atom_concat(URL, Year, U),
    http_client:http_get(U, R, []),
    atom_json_term(R, J, []),
    J=json(ResponseFields),
    (memberchk(status='OK', ResponseFields) ; memberchk(status='SUCCESS', ResponseFields)),
    memberchk(results=L, ResponseFields),
    % Remove duplicates and sort by ice_out_date, name, and id for consistent ordering
    remove_duplicate_records(L, Unique),
    sort_records(Unique, Sorted),
    maplist(store_record, Sorted).

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
    format(atom(Title), 'Minnesota Latitude : ~D N', [Lat]),
    context_r:rplot_with_regression(FN, Years, Times, Title, 'year', 'iceOutDay', Slope),
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


