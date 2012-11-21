:- module(context_map, []).

/** <module> Context model map display
    * Uses marker to center locale
    * Multiple marker
    * Search
    * Clickable map
*/

:- context:register(context_map:navigate).
:- context:register(context_map:search).
:- context:register(context_map:navigate_locale).


navigate(Request) :-
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



navigate_locale(Request) :-
   http_parameters(Request, [locale(Locale, [string]),
			     action(Action, [])
			    ]),
   Action = 'Map',
   rdfR(Locale, ent:lat, Lat),
   rdfR(Locale, ent:lon, Lon),
   print(user_error, [Lat, Lon, Locale]),
   reply_html_page(
	    [title('Map Home')],
	    [
	     \(context_graphing:map_native(Lat, Lon, Locale))
	    ]
                  ).

navigate_locale(Request) :-
   http_parameters(Request, [locale(Locale, [string]),
			     action(Action, [])
			    ]),
   Action = 'Available',
   rdfR(Locale, ent:lat, Lat),
   rdfR(Locale, ent:lon, Lon),
   print(user_error, [Lat, Lon, Locale]),
   reply_html_page(
	    [title('Map Home')],
	    [
	     p([Locale, ' is used here:'])
	    ]
                  ).

find_locs(option([value(Out)],[Name])) :-
   rdf(Out, ent:lat, _),
   rdf(Out, ent:lon, _),
   rdf(Out, ent:title, Name).

search(Request) :-
    setof(Loc, Loc ^ find_locs(Loc), List),
    reply_html_page(
        cliopatria(default),
        [title('Map Search')],
        [
         \(con_text:table_with_iframe_target(
               Request,
               [
                h1('Context map/location view'),
		ul( li(p([\(con_text:gif(map)),'Select a modeled locale to map',
			  form([action('navigate_locale'), target(target_iframe)],
			       [
				select([name('locale')], List),
				input([type('submit'), name('action'), value('Map')]),
				input([type('submit'), name('action'), value('Available')])
			       ]
			      )
			 ]
			)
		      )
		  ),
                p('After submitted, click on marker to magnify'),
                h1(' '),
		\(render_iframe(render) )
               ]
                                   )
          )
        ]
                   ).

% USA map

/*
max_lat(52.5).
min_lat(23.5).
max_lon(-52.5).
min_lon(-125.5).
*/

max_lat(49.0).
min_lat(20.0).
max_lon(-65.0).
min_lon(-129.0).


image_width(350).   %250
image_height(210).  %150

get_ll_extent(Lat, Lon, X, Y, W, H) :-
    max_lat(MaxLat),
    min_lat(MinLat),
    max_lon(MaxLon),
    min_lon(MinLon),
    image_width(IW),
    image_height(IH),
    DiffLon is integer(MaxLon-MinLon),
    DiffLat is integer(MaxLat-MinLat),
    Y is IH-integer(IH*(Lat-MinLat)/DiffLat),
    X is integer(IW*(Lon-MinLon)/DiffLon),
    H is integer(IH/DiffLat) + Y,
    W is integer(IW/DiffLon) + X.

section_name(Lat, Lon, Title) :-
    rdfR(Ent, ent:lat_min, Lat),
    rdfR(Ent, ent:lon_min, Lon),
    rdfS(Ent, ent:name, Title), !.
%    context_geo:find_dem_section(Lat, Lon, Title), !.
section_name(_Lat, _Lon, 'missing').

lon_regions(_URI, _Target, _Lat, Lon) -->
    {
     min_lon(MinLon),
     Lon < MinLon
    }.
lon_regions(URI,Target,Lat, Lon) -->
    {
     format(atom(Href), URI, % 'contour?lat=~w&lon=~w'
            [Lat, Lon]),
     get_ll_extent(Lat, Lon, X, Y, W, H),
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
    lon_regions(URI, Target, Lat, NewLon).

lat_regions(_URI, _Target, Lat, _Lon) -->
    {
     min_lat(MinLat),
     Lat < MinLat
    }.
lat_regions(URI, Target, Lat, Lon) -->
    {
       NewLat is Lat - 1.0
    },
    lon_regions(URI, Target, Lat, Lon),
    lat_regions(URI, Target, NewLat, Lon).

usa_map(URI,Target) -->
    {
     max_lat(MaxLat),
     max_lon(MaxLon),
     image_width(ImageWidth),
     image_height(ImageHeight)
    },
    html([
        img([width=ImageWidth, height=ImageHeight, name=usa0,
             usemap='#usa', src('/html/images/usa.gif')]),
        map([name=usa],[
                        \(lat_regions(URI, Target, MaxLat, MaxLon))
                       ])
         ]).

