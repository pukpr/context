:- module(context_map, [get_location/4,
		        available_location/4,
                        find_example/3]).

/** <module> Context model map display
    * Uses marker to center locale
    * Multiple marker
    * Search
    * Clickable map
*/

:- context:register(context_map:navigate).
:- context:register(context_map:view).
:- context:register(context_map:navigate_locale).
:- http_handler('/context_map/locale', find_locale, []).

% :- context:ac_hook(context_map:locale).

%%   minutes_to_degrees(+DMS, -Degrees)
%
%    Convert degreesminutesseconds to frational degrees
minutes_to_degrees((Deg,Min,Sec), Degrees) :-
     (	 Deg < 0.0 ->
         Degrees is Deg - Min/60 - Sec/3600
     ;
        Degrees is Deg + Min/60 + Sec/3600
     ).


%%   get_location(+URI, -Lat, -Lon, -Title)
%
%    Get location for a URI
get_location(URI, Lat, Lon, Title) :-
    rdfS(URI, ent:title, Title),
    rdfR(URI, ent:lat, Lat),
    rdfR(URI, ent:lon, Lon),
    !.
get_location(URI, Lat, Lon, Title) :-
    rdfS(URI, ent:title, Title),
    rdfL(URI, ent:lat, Lat0), minutes_to_degrees(Lat0, Lat),
    rdfL(URI, ent:lon, Lon0), minutes_to_degrees(Lon0, Lon),
    !.
/*
get_location(Title, Lat, Lon, URI) :-
    rdfR(URI, ent:lat, Lat),
    rdfR(URI, ent:lon, Lon),
    rdfS(URI, ent:title, Title), !.

get_location(URI, 0.0, 0.0, Title) :-
    rdfS(URI, ent:title, Title).
*/



%%   view(+Request)
%
%    View map of locale
view(Request) :-
   http_parameters(Request, [lat(Lat, [float]),
			     lon(Lon, [float]),
			     title(Title, [string])
			    ]
		              ),

   reply_html_page(
	    [title('Map Home')],
	    [
	     \(context_graphing:map_native(Lat, Lon, Title))
	    ]
		       ).

%%   available_location(+Locale, -Title, -Model, -Feature)
%
%    Deteremines if locale has available features
available_location(Locale, Title, Model, Feature) :-
     rdfS(Locale, ent:title, Title),
     rdf(URI, ent:locale, Locale),
     rdfS(URI, ent:feature, Feature),
     ref_m(Feature, model, Model).
available_location(Locale, Title, Model, Feature) :-
     rdfS(Locale, ent:title, Title),
     % rdf(Locale, ent:locale, Locale),
     rdfS(Locale, ent:feature, Feature),
     ref_m(Feature, model, Model).
% available_location(Locale, Locale, '#', Locale).


%%   navigate(+Request)
%
%    Dynamic page to geospatial search
%%   navigate_locale(+Request)
%
%    Dynamic page to geospatial location search
navigate_locale(Request) :-
   http_parameters(Request, [locale(Locale_or_Title, [string]),
			     action(Action, [])
			    ]),
   Action = 'Map',
   (
    available_location(Locale_or_Title, _Title, _Model, _Feature) ->
    Locale = Locale_or_Title,
    get_location(Locale, Lat, Lon, Title)
   ;
    Title = Locale_or_Title,
    get_location(Locale, Lat, Lon, Title)
   ),
   % get_location(Locale, Lat, Lon, Title),
   % print('user_error', ['=============A ', Locale,Title, '============']),
   % print(user_error, [Lat, Lon, Locale]),
   reply_html_page(
	    [title('Map Home')],
	    [
	     \(context_graphing:map_native(Lat, Lon, Title))
	    ]
                  ).


navigate_locale(Request) :-
   http_parameters(Request, [locale(Locale_or_Title, [string]),
			     action(Action, [])
			    ]),
   Action = 'Available',
   (
    available_location(Locale_or_Title, Title, _, _) ->
    Locale = Locale_or_Title
   ;
    available_location(Locale, Locale_or_Title, _, _) ->
    Title = Locale_or_Title
   ;
    Title = 'Press "Available" to determine what' ,
    Locale = Locale_or_Title
   ),
   findall(li(a([href(Model),
	      target('_parent')],
	     Feature)),
	   available_location(Locale, _, Model, Feature),
	   Models),
   reply_html_page(
	    [title('Map Home')],
	    [
	     p(b(i([Title, ' location is used in the context of :']))),
	     ul(
	        Models
	       )
	    ]
                  ).

%%   find_loc(Query, URI, Title)
%
%    Finds matches to query
%%   find_locs(-Option)
%
%    Generates selection options
find_locs(option([value(Out)],[Name])) :-
   rdf(Out, ent:lat, _),
   rdf(Out, ent:lon, _),
   rdf(Out, ent:title, Name).

find_loc(Query, URI, Title) :-
   rdf(URI, ent:lat, _),
   rdf(URI, ent:lon, _),
   rdf(URI, ent:title, literal(substring(Query),Title)).

%%   find_example(+Query, -URI, -Title)
%
%    Finds matches to query, same as *find_loc*
find_example(Query, URI, Title) :-
   rdf(URI, ent:lat, _),
   rdf(URI, ent:lon, _),
   rdf(URI, ent:title, literal(substring(Query),Title)).


%%   autocompletions(+Query, +Max, -Count, -Completions)
%
%    Autocompletion query
autocompletions(Query, Max, Count, Completions) :-
    print(user_error,['Q', Query]),
    findall([URI,Type], find_loc(Query, URI, Type), Completions0),
    sort(Completions0, Completions1),
    length(Completions1, Count),
    con_text:first_n(Max, Completions1, Completions2),
    maplist(ac_result, Completions2, Completions).


ac_result([Obj,Type], json([ label=Type,
                              type=Obj,
                              href=HREF
                            ])) :-
    % replace_word('#', '%23', Obj, Available),
    % print(user_error, ['URL', Obj, ' | ', Available]), !,
    atomic_list_concat(['/context_map/navigate_locale?action=Available&locale=',Obj],URL),
    atomic_list_concat(['javascript:retarget_frame(this,"', URL, '", "render");'], HREF).
    % print(user_error, ['HREF', HREF]).

%%   find_locale(+Request)
%
%    Dynamic page for locale
find_locale(Request) :-
    http_parameters(Request,
                    [ query(Query, [description('Typed string')]),
                      maxResultsDisplayed(Max, [integer, default(100), description('Max number of results to show') ])
                    ]),
    autocompletions(Query, Max, Count, Completions),
    reply_json(json([ query = json([ count=Count ]),
                      results = Completions
                    ])).


navigate(Request) :-
    setof(Loc, Loc ^ find_locs(Loc), LocList),
    sort(LocList, List),
    reply_html_page(
        cliopatria(default),
        [title('Map Search'),
	 script([type('text/javascript'),src('/html/js/submit.js')], [])
	],
        [
         \(con_text:table_with_iframe_target(
               Request,
               [
                h1('Context map/location view'),
		ul( li(p([\(con_text:gif(map)),'Select a modeled locale to map, or check for feature availability',
                          form([action('navigate_locale'), target(target_iframe)],
			       [
				select([name('locale')], List),
				input([type('submit'), name('action'), value('Map'),
				       onclick('subm(this.form,"target_iframe");')]),
				input([type('submit'), name('action'), value('Available'),
				       onclick('subm(this.form,"render");')])
			       ]
			      ),
                          br([]),
                          form([action('navigate_locale'), target(target_iframe)],
			       [
				% select([name('locale')], List),
                                \(con_text:ac(find_locale, locale)),
                                br([]),
				input([type('submit'), name('action'), value('Map'),
				       onclick('subm(this.form,"target_iframe");')]),
				input([type('submit'), name('action'), value('Available'),
				       onclick('subm(this.form,"render");')])
			       ]
			      ),

                          br([])

                          /*
                          form([action('navigate_locale'), target(target_iframe)],
			       [
                                \(con_text:autoc(find_example, locale)),
                                br([]),
				input([type('submit'), name('action'), value('Map'),
				       onclick('subm(this.form,"target_iframe");')]),
				input([type('submit'), name('action'), value('Available'),
				       onclick('subm(this.form,"render");')])
			       ]
			      )
                          */


			 ]
			)
		      )
		  ),
                %p('After submitted, click on marker to magnify'),
                h1(' '),
		\(render_iframe(render) )
               ]
                                   )
          )
        ]
                   ).

% USA map

/*
%%   max_lat(-Lat)
%
%    Fixed Max
max_lat(52.5).
%%   min_lat(-Lat)
%
%    Fixed Min
min_lat(23.5).
%%   max_lon(-Lon)
%
%    Fixed Max
max_lon(-52.5).
%%   min_lon(-Lon)
%
%    Fixed Min
min_lon(-125.5).
*/

max_lat(usa,49.0).
max_lat(mn,49.0).
min_lat(usa,20.0).
min_lat(mn,42.0).
max_lon(usa,-65.0).
max_lon(mn,-89.0).
min_lon(usa,-129.0).
min_lon(mn,-96.0).


%%   image_width(-W)
%
%    Fixed Width
image_width(usa,350).   %250
image_width(mn,210).
%%   image_height(-H)
%
%    Fixed Height
image_height(usa,210).  %150
image_height(mn,210).

%%   get_ll_extent(+Lat, +Lon, +X, +Y, +W, +H)
%
%    Get LatLon extents as rectilinear coordinates for mapping
get_ll_extent(Lat, Lon, X, Y, W, H,G) :-
    max_lat(G,MaxLat),
    min_lat(G,MinLat),
    max_lon(G,MaxLon),
    min_lon(G,MinLon),
    image_width(G,IW),
    image_height(G,IH),
    DiffLon is integer(MaxLon-MinLon),
    DiffLat is integer(MaxLat-MinLat),
    Y is IH-integer(IH*(Lat-MinLat)/DiffLat),
    X is integer(IW*(Lon-MinLon)/DiffLon),
    H is integer(IH/DiffLat) + Y,
    W is integer(IW/DiffLon) + X.

%%   section_name(+Lat, +Lon, -Title)
%
%    Section name for LatLon
section_name(Lat, Lon, Title) :-
    rdfR(Ent, ent:lat_min, Lat),
    rdfR(Ent, ent:lon_min, Lon),
    rdfS(Ent, ent:name, Title), !.
%    context_geo:find_dem_section(Lat, Lon, Title), !.
section_name(_Lat, _Lon, 'missing').

%%   lon_regions(+URI, +Target, +Lat, +Lon)//
%
%    Inline Longitudinal regions
lon_regions(_URI, _Target, _Lat, Lon, MinLon, _MinLat,_G) -->
    {
     % min_lon(MinLon),
     Lon < MinLon
    }.
lon_regions(URI,Target,Lat, Lon, MinLon, MinLat,G) -->
    {
     format(atom(Href), URI, % 'contour?lat=~w&lon=~w'
            [Lat, Lon]),
     get_ll_extent(Lat, Lon, X, Y, W, H,G),
     format(atom(Coords), '~d,~d,~d,~d', [X,Y,W,H]),
     section_name(Lat, Lon, Title),
     NewLon is Lon - 1.0
    },
    html(area([shape=rect,
               coords=Coords,
               title=Title,
               href=Href,
               target=Target % 'results'
              ])),
    lon_regions(URI, Target, Lat, NewLon, MinLon, MinLat,G).

%%   lat_regions(+URI, +Target, +Lat, +Lon)//
%
%    Inline Latitudinal regions
lat_regions(_URI, _Target, Lat, _Lon, _MinLon, MinLat,_G) -->
    {
     % min_lat(MinLat),
     Lat < MinLat
    }.
lat_regions(URI, Target, Lat, Lon, MinLon, MinLat,G) -->
    {
       NewLat is Lat - 1.0
    },
    lon_regions(URI, Target, Lat, Lon, MinLon, MinLat,G),
    lat_regions(URI, Target, NewLat, Lon, MinLon, MinLat,G).

%%   usa_map(+URI,+Target)//
%
%    Inline Map of USA to use as selection
usa_map(URI,Target) -->
    {
     max_lat(usa,MaxLat),
     max_lon(usa,MaxLon),
     min_lon(usa,MinLon),
     min_lat(usa,MinLat),
     image_width(usa,ImageWidth),
     image_height(usa,ImageHeight)
    },
    html([
        img([width=ImageWidth, height=ImageHeight, name=usa0,
             usemap='#usa', src('/html/images/usa.gif')]),
        map([name=usa],[
                        \(lat_regions(URI, Target, MaxLat, MaxLon, MinLon, MinLat,usa))
                       ])
         ]).




minnesota_map(URI,Target) -->
    {
     max_lat(mn,MaxLat),
     max_lon(mn,MaxLon),
     min_lon(mn,MinLon),
     min_lat(mn,MinLat),
     image_width(mn,ImageWidth),
     image_height(mn,ImageHeight)
    },
    html([
        img([width=ImageWidth, height=ImageHeight, name=mn0,
             usemap='#minnesota', src('/html/images/minnesota.gif')]),
        map([name=minnesota],[
                        \(lat_regions(URI, Target, MaxLat, MaxLon, MinLon, MinLat,mn))
                       ])
         ]).
