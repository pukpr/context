:- module(context_geo, []).

:- use_module(context_math).
:- use_module(library(clpfd)).

    /* Ellipsoid model constants (actual values here are for WGS84) */

    sm_a(6378137.0).
    sm_b(6356752.314).
    sm_EccSquared(6.69437999013e-03).
    theUTMScaleFactor(0.9996).


    /*
    * DegToRad
    *
    * Converts degrees to radians.
    *
    */
    degToRad(Deg, Rad) :-
        Rad is (Deg / 180.0 * pi).


    /*
    * RadToDeg
    *
    * Converts radians to degrees.
    *
    */
    radToDeg(Rad, Deg) :-
        Deg is (Rad / pi * 180.0).


    /*
    * ArcLengthOfMeridian
    *
    * Computes the ellipsoidal distance from the equator to a point at a
    * given latitude.
    *
    * Reference: Hoffmann-Wellenhof, B., Lichtenegger, H., and Collins, J.,
    * GPS: Theory and Practice, 3rd ed.  New York: Springer-Verlag Wien, 1994.
    *
    * Inputs:
    *     phi - Latitude of the point, in radians.
    *
    * Globals:
    *     sm_a - Ellipsoid model major axis.
    *     sm_b - Ellipsoid model minor axis.
    *
    * Returns:
    *     The ellipsoidal distance of the point from the equator, in meters.
    *
    */

    arcLengthOfMeridian(Phi, ArcLength) :-
        % var alpha, beta, gamma, delta, epsilon, n
        % var result

        /* Precalculate n */
        sm_a(SM_A),
        sm_b(SM_B),

        N is (SM_A - SM_B) / (SM_A + SM_B),

        /* Precalculate alpha */
        Alpha is ((SM_A + SM_B) / 2) * (1 + (N^2) / 4) + (N^4) / 64,

        /* Precalculate beta */
        Beta is (-3 * N / 2) + (9 * (N^3) / 16) + (-3 * (N^5) / 32),

        /* Precalculate gamma */
        Gamma is (15 * (N^2) / 16) + (-15 * (N^4) / 32),

        /* Precalculate delta */
        Delta is (-35 * (N^3) / 48.0) + (105.0 * (N^5) / 256),

        /* Precalculate epsilon */
        Epsilon is (315.0 * (N^4.0) / 512.0),

       /* Now calculate the sum of the series and return */
       ArcLength is Alpha
          * (Phi + (Beta * sin(2 * Phi))
            + (Gamma * sin(4 * Phi))
            + (Delta * sin(6 * Phi))
            + (Epsilon * sin(8 * Phi))).




    /*
    * UTMCentralMeridian
    *
    * Determines the central meridian for the given UTM zone.
    *
    * Inputs:
    *     zone - An integer value designating the UTM zone, range [1,60].
    *
    * Returns:
    *   The central meridian for the given UTM zone, in radians, or zero
    *   if the UTM zone parameter is outside the range [1,60].
    *   Range of the central meridian is the radian equivalent of [-177,+177].
    *
    */
    utmCentralMeridian(Zone,Cmeridian) :-
        % var cmeridian
        Temp is -183.0 + (Zone * 6.0),
        degToRad(Temp, Cmeridian).


    /*
    * FootpointLatitude
    *
    * Computes the footpoint latitude for use in converting transverse
    * Mercator coordinates to ellipsoidal coordinates.
    *
    * Reference: Hoffmann-Wellenhof, B., Lichtenegger, H., and Collins, J.,
    *   GPS: Theory and Practice, 3rd ed.  New York: Springer-Verlag Wien, 1994.
    *
    * Inputs:
    *   y - The UTM northing coordinate, in meters.
    *
    * Returns:
    *   The footpoint latitude, in radians.
    *
    */
    footpointLatitude(Y, Lat) :-
        % var y_, alpha_, beta_, gamma_, delta_, epsilon_, n
        % var result
        sm_a(SM_A),
        sm_b(SM_B),

        /* Precalculate n (Eq. 10.18) */
        N is (SM_A - SM_B) / (SM_A + SM_B),

        /* Precalculate alpha_ (Eq. 10.22) */
        /* (Same as alpha in Eq. 10.17) */
        Alpha_ is ((SM_A + SM_B) / 2) * (1 + ((N^2) / 4) + ((N^4) / 64)),

        /* Precalculate y_ (Eq. 10.23) */
        Y_ is Y / Alpha_,

        /* Precalculate beta_ (Eq. 10.22) */
        Beta_ is (3 * N / 2.0) + (-27 * (N^3) / 32) + (269.0 * (N^5) / 512),

        /* Precalculate gamma_ (Eq. 10.22) */
        Gamma_ is (21.0 * (N^2) / 16.0) + (-55.0 * (N^4) / 32),

        /* Precalculate delta_ (Eq. 10.22) */
        Delta_ is (151.0 * (N^3) / 96) + (-417.0 * (N^5) / 128),

        /* Precalculate epsilon_ (Eq. 10.22) */
        Epsilon_ is (1097.0 * (N^4) / 512.0),

        /* Now calculate the sum of the series (Eq. 10.21) */
        Lat is Y_ + (Beta_ * sin(2.0 * Y_))
            + (Gamma_ * sin(4.0 * Y_))
            + (Delta_ * sin(6.0 * Y_))
            + (Epsilon_ * sin(8.0 * Y_)).



    /*
    * MapLatLonToXY
    *
    * Converts a latitude/longitude pair to x and y coordinates in the
    * Transverse Mercator projection.  Note that Transverse Mercator is not
    * the same as UTM -- a scale factor is required to convert between them.
    *
    * Reference: Hoffmann-Wellenhof, B., Lichtenegger, H., and Collins, J.,
    * GPS: Theory and Practice, 3rd ed.  New York: Springer-Verlag Wien, 1994.
    *
    * Inputs:
    *    phi - Latitude of the point, in radians.
    *    lambda - Longitude of the point, in radians.
    *    lambda0 - Longitude of the central meridian to be used, in radians.
    *
    * Outputs:
    *    xy - A 2-element array containing the x and y coordinates
    *         of the computed point.
    *
    * Returns:
    *    The function does not return a value.
    *
    */
    mapLatLonToXY(Phi, Lambda, Lambda0, [X,Y]) :-
        % var N, nu2, ep2, t, t2, l
        % var l3coef, l4coef, l5coef, l6coef, l7coef, l8coef
        % var tmp
        sm_a(SM_A),
        sm_b(SM_B),

        /* Precalculate ep2 */
        Ep2 is ((SM_A^2) - (SM_B^2)) / (SM_B^2),

        /* Precalculate nu2 */
        Nu2 is Ep2 * ((cos(Phi))^2),

        /* Precalculate N */
        N is (SM_A^2) / (SM_B*sqrt(1 + Nu2)),

        /* Precalculate t */
        T is tan(Phi),
        T2 is T * T,
        % Tmp is (T2 * T2 * T2) - (T^6),

        /* Precalculate l */
        L is Lambda - Lambda0,

        /* Precalculate coefficients for l**n in the equations below
           so a normal human being can read the expressions for easting
           and northing
           -- l**1 and l**2 have coefficients of 1.0 */
        L3coef is 1.0 - T2 + Nu2,

        L4coef is 5.0 - T2 + 9 * Nu2 + 4.0 * (Nu2 * Nu2),

        L5coef is 5.0 - 18.0 * T2 + (T2 * T2) + 14.0 * Nu2
            - 58.0 * T2 * Nu2,

        L6coef is 61.0 - 58.0 * T2 + (T2 * T2) + 270.0 * Nu2
            - 330.0 * T2 * Nu2,

        L7coef is 61.0 - 479.0 * T2 + 179.0 * (T2 * T2) - (T2 * T2 * T2),

        L8coef is 1385.0 - 3111.0 * T2 + 543.0 * (T2 * T2) - (T2 * T2 * T2),

    /* Precalculate cos (phif) */
        CP is cos(Phi),

        /* Calculate easting (x) */
        X is N * CP * L
            + (N / 6.0 * (CP^3) * L3coef * (L^3))
            + (N / 120.0 * (CP^5) * L5coef * (L^5))
            + (N / 5040.0 * (CP^7) * L7coef * (L^7)),

        /* Calculate northing (y) */

        arcLengthOfMeridian(Phi, AL),

        Y is AL
            + (T / 2.0 * N * (CP^2) * (L^2))
            + (T / 24.0 * N * (CP^4) * L4coef * (L^4))
            + (T / 720.0 * N * (CP^6) * L6coef * (L^6))
            + (T / 40320.0 * N * (CP^8) * L8coef * (L^8)).



    /*
    * MapXYToLatLon
    *
    * Converts x and y coordinates in the Transverse Mercator projection to
    * a latitude/longitude pair.  Note that Transverse Mercator is not
    * the same as UTM -- a scale factor is required to convert between them.
    *
    * Reference: Hoffmann-Wellenhof, B., Lichtenegger, H., and Collins, J.,
    *   GPS: Theory and Practice, 3rd ed.  New York: Springer-Verlag Wien, 1994.
    *
    * Inputs:
    *   x - The easting of the point, in meters.
    *   y - The northing of the point, in meters.
    *   lambda0 - Longitude of the central meridian to be used, in radians.
    *
    * Outputs:
    *   philambda - A 2-element containing the latitude and longitude
    *               in radians.
    *
    * Returns:
    *   The function does not return a value.
    *
    * Remarks:
    *   The local variables Nf, nuf2, tf, and tf2 serve the same purpose as
    *   N, nu2, t, and t2 in MapLatLonToXY, but they are computed with respect
    *   to the footpoint latitude phif.
    *
    *   x1frac, x2frac, x2poly, x3poly, etc. are to enhance readability and
    *   to optimize computations.
    *
    */
    mapXYToLatLon(X, Y, Lambda0, [Phi,Lambda]) :-
        % var phif, Nf, Nfpow, nuf2, ep2, tf, tf2, tf4, cf
        % var x1frac, x2frac, x3frac, x4frac, x5frac, x6frac, x7frac, x8frac
        % var x2poly, x3poly, x4poly, x5poly, x6poly, x7poly, x8poly
        sm_a(SM_A),
        sm_b(SM_B),

        /* Get the value of phif, the footpoint latitude. */
        footpointLatitude(Y, Phif),

        /* Precalculate ep2 */
        Ep2 is ((SM_A^2) - (SM_B^2)) / (SM_B^2),

        /* Precalculate cos (phif) */
        Cf is cos(Phif),

        /* Precalculate nuf2 */
        Nuf2 is Ep2 * (Cf^2),

        /* Precalculate Nf and initialize Nfpow */
        Nf is (SM_A^2) / (SM_B * sqrt(1 + Nuf2)),
        Nfpow is Nf,

        /* Precalculate tf */
        Tf is tan(Phif),
        Tf2 is Tf * Tf,
        Tf4 is Tf2 * Tf2,

        /* Precalculate fractional coefficients for x**n in the equations
           below to simplify the expressions for latitude and longitude. */
        X1frac is 1.0 / (Nfpow * Cf),

        Nfpow1 is Nfpow*Nf,   % now equals Nf**2)
        X2frac is Tf / (2.0 * Nfpow1),

        Nfpow2 is Nfpow1*Nf,   % now equals Nf**3)
        X3frac is 1.0 / (6.0 * Nfpow2 * Cf),

        Nfpow3 is Nfpow2*Nf,   % now equals Nf**4)
        X4frac is Tf / (24.0 * Nfpow3),

        Nfpow4 is Nfpow3*Nf,   % now equals Nf**5)
        X5frac is 1.0 / (120.0 * Nfpow4 * Cf),

        Nfpow5 is Nfpow4*Nf,   % now equals Nf**6)
        X6frac is Tf / (720.0 * Nfpow5),

        Nfpow6 is Nfpow5*Nf,   % now equals Nf**7)
        X7frac is 1.0 / (5040.0 * Nfpow6 * Cf),

        Nfpow7 is Nfpow6*Nf,   /* now equals Nf**8) */
        X8frac is Tf / (40320.0 * Nfpow7),

        /* Precalculate polynomial coefficients for x**n.
           -- x**1 does not have a polynomial coefficient. */
        X2poly is -1.0 - Nuf2,

        X3poly is -1.0 - 2 * Tf2 - Nuf2,

        X4poly is 5.0 + 3.0 * Tf2 + 6.0 * Nuf2 - 6.0 * Tf2 * Nuf2
		- 3.0 * (Nuf2 *Nuf2) - 9.0 * Tf2 * (Nuf2 * Nuf2),

        X5poly is 5.0 + 28.0 * Tf2 + 24.0 * Tf4 + 6.0 * Nuf2 + 8.0 * Tf2 * Nuf2,

        X6poly is -61.0 - 90.0 * Tf2 - 45.0 * Tf4 - 107.0 * Nuf2
		+ 162.0 * Tf2 * Nuf2,

        X7poly is -61.0 - 662.0 * Tf2 - 1320.0 * Tf4 - 720.0 * (Tf4 * Tf2),

        X8poly is 1385.0 + 3633.0 * Tf2 + 4095.0 * Tf4 + 1575 * (Tf4 * Tf2),

        /* Calculate latitude */
        Phi is Phif + X2frac * X2poly * (X * X)
		+ X4frac * X4poly * (X^4)
		+ X6frac * X6poly * (X^6)
		+ X8frac * X8poly * (X^8),

        /* Calculate longitude */
        Lambda is Lambda0 + X1frac * X
		+ X3frac * X3poly * (X^3)
		+ X5frac * X5poly * (X^5)
		+ X7frac * X7poly * (X^7).




    /*
    * LatLonToUTMXY
    *
    * Converts a latitude/longitude pair to x and y coordinates in the
    * Universal Transverse Mercator projection.
    *
    * Inputs:
    *   lat - Latitude of the point, in radians.
    *   lon - Longitude of the point, in radians.
    *   zone - UTM zone to be used for calculating values for x and y.
    *          If zone is less than 1 or greater than 60, the routine
    *          will determine the appropriate zone from the value of lon.
    *
    * Outputs:
    *   xy - A 2-element array where the UTM x and y values will be stored.
    *
    * Returns:
    *   The UTM zone used for calculating the values of x and y.
    *
    */
    latLonToUTMXY(Lat, Lon, Zone, [X,Y]) :-
        utmCentralMeridian(Zone, Cmeridian),
        mapLatLonToXY(Lat, Lon, Cmeridian, [X0,Y0]),
        theUTMScaleFactor(UTMScaleFactor),

        /* Adjust easting and northing for UTM system. */
        X is X0 * UTMScaleFactor + 500000.0,
        Y1 is Y0 * UTMScaleFactor,
        (   Y1 < 0.0 ->
        Y is Y1 + 10000000.0;
        Y = Y1
        ).





    /*
    * UTMXYToLatLon
    *
    * Converts x and y coordinates in the Universal Transverse Mercator
    * projection to a latitude/longitude pair.
    *
    * Inputs:
    *	x - The easting of the point, in meters.
    *	y - The northing of the point, in meters.
    *	zone - The UTM zone in which the point lies.
    *	southhemi - True if the point is in the southern hemisphere,
    *               false otherwise.
    *
    * Outputs:
    *	latlon - A 2-element array containing the latitude and
    *            longitude of the point, in radians.
    *
    * Returns:
    *	The function does not return a value.
    *
    */
    utmXYToLatLon(X, Y, Zone, Southhemi, [Lat,Lon]) :-
        % var cmeridian
        theUTMScaleFactor(UTMScaleFactor),

        X0 is X - 500000.0,
        X1 is X0/ UTMScaleFactor,

        /* If in southern hemisphere, adjust y accordingly. */
        (   Southhemi = true ->
        Y0 is Y - 10000000.0;
        Y0 = Y
        ),
        Y1 is Y0/UTMScaleFactor,

        utmCentralMeridian(Zone, Cmeridian),
        mapXYToLatLon(X1, Y1, Cmeridian, [Lat,Lon]).

ll_to_utm(Lat, Lon, Zone, Easting, Northing) :-
    degToRad(Lon,LonR),
    degToRad(Lat,LatR),
    latLonToUTMXY(LatR, LonR, Zone, [Easting,Northing]).

zone_from_section_designator(Info, Zone) :-
    atom_codes(Info, [_,_,A,B|_]),
    number_to_chars( Zone, [A,B]), !.
zone_from_section_designator(_Info, -999).

% ----------------------------------------------------

% Elevation section
%
% Latitude and Longitude

get_elevation(Lat, Lon, Elev) :-

   format(atom(S),
          'http://maps.googleapis.com/maps/api/elevation/json?locations=~w,~w&sensor=false',
          [Lat, Lon]),
   http_client:http_get(S, Response, []),!,
   % print(user_error, Response),
   Response=json([results=[json([elevation=Elev,
                          location=json([lat=_Lat,
                                         lng=_Lon]),
                          resolution=_])
                   ],
           status='OK']).

genRequestPairs([[Lat],[Lon]], Li, List) :-
    atomic_list_concat([Lat, ',', Lon, '%7C', Li], List).
genRequestPairs([[Lat|LatRest],
		 [Lon|LonRest]], '', List) :-
    atomic_list_concat([Lat, ',', Lon], Next),
    genRequestPairs([LatRest,LonRest], Next, List).
genRequestPairs([[Lat|LatRest],
		 [Lon|LonRest]], Li, List) :-
    atomic_list_concat([Lat, ',', Lon, '%7C',Li], Next),
    genRequestPairs([LatRest,LonRest], Next, List).

collectResponse([], List, List).
collectResponse([F|R], Li, List) :-
    F =  json([elevation=Elev,_,_]),
    collectResponse(R, [Elev|Li], List).


get_elevation(LatLonList, ElevList) :-
   genRequestPairs(LatLonList, '', QueryList),
   format(atom(S),
          'http://maps.googleapis.com/maps/api/elevation/json?locations=~w&sensor=false',
          QueryList),
   print(user_error, 'calling google:'),
   http_client:http_get(S, Response, []),!,
   print(user_error, Response),
   Response=json([results=List,
		  status='OK']),
   collectResponse(List, [], ElevList).

get_elevation_path(LatLonList, Samples, ElevList) :-
   genRequestPairs(LatLonList, '', QueryList),
   format(atom(S),
          'http://maps.googleapis.com/maps/api/elevation/json?path=~w&samples=~w&sensor=false',
          [QueryList, Samples]),
   print(user_error, 'calling google:'),
   http_client:http_get(S, Response, []),!,
   print(user_error, Response),
   Response=json([results=List,
		  status='OK']),
   collectResponse(List, [], ElevList).

%  UTM  Universal Transverse Mercator

get_elevation(Easting, Northing, Zone, Southhemi, Elev) :-
   utmXYToLatLon(Easting, Northing, Zone, Southhemi, [Lat,Lon]),
   radToDeg(Lat, Latitude),
   radToDeg(Lon, Longitude),
   get_elevation(Latitude, Longitude, Elev).

convert_lat_lon([[],[]], _, _, List, List).
convert_lat_lon([[E|ERest],
		 [N|NRest]], Zone, South, [Lats,Lons], List) :-
   utmXYToLatLon(E, N, Zone, South, [Latitude,Longitude]),
   radToDeg(Latitude, Lat),
   radToDeg(Longitude, Lon),
   convert_lat_lon([ERest,NRest], Zone, South, [[Lat|Lats],[Lon|Lons]], List).

get_elevation(EastingNorthingList, Zone, Southhemi, Elev) :-
   convert_lat_lon(EastingNorthingList, Zone, Southhemi, [[], []], LatLonList),
   get_elevation(LatLonList, Elev).

get_elevation_path(EastingNorthingList, Zone, Southhemi, Samples, Elev) :-
   convert_lat_lon(EastingNorthingList, Zone, Southhemi, [[], []], LatLonList),
   get_elevation_path(LatLonList, Samples, Elev).

:- dynamic
    dem/2,
    demElev/3,
    diffElev/3,
    diffElev/4.


% need to load something in dem/2  first.
%
get_elevation_current_dem(X,Y,Elev) :-
    dem(_,YL),
    nth0(Y,YL,XL),
    nth0(X,XL,Elev).

find_dem_section(Lat, Lon, Name, Info) :-
    var(Lat),
    var(Lon),
    rdfS(Ent, ent:name, Name),
    rdfS(Ent, ent:section_designator, Info),
    rdfR(Ent, ent:lat_min, Lat),
    rdfR(Ent, ent:lon_min, Lon).
find_dem_section(Lat, Lon, Name) :-
    rdfR(Ent, ent:lat_min, LAMI),
    rdfR(Ent, ent:lat_max, LAMA),
    rdfR(Ent, ent:lon_min, LOMI),
    rdfR(Ent, ent:lon_max, LOMA),
    Lat < LAMA, Lat >= LAMI,
    Lon < LOMA, Lon >= LOMI,
    rdfS(Ent, ent:name, Name).

store_dem_pos(_Name, [[]], _, _) :-  !.
store_dem_pos(_Name, [], _, _) :- !.
% store_dem_pos(_Name, [], _, _).
store_dem_pos(Name, [First|Cols], RowNum, ColNum) :-
    assert(demElev(% Name,
                   RowNum,ColNum,First)),
    NextCol is ColNum + 1,
    !,
    store_dem_pos(Name, Cols, RowNum, NextCol).

store_dem_row(_Name, [[]], _) :- !.
store_dem_row(_Name, [], _) :- !.
store_dem_row(Name, [First|Rows], RowNum) :-
    NextRow is RowNum + 1,
    !,
    store_dem_pos(Name, First, RowNum, 0),
    store_dem_row(Name, Rows, NextRow).

prep_dem_section(Name) :-
    cleanup_dem,
    dem(Name,List),
    store_dem_row(Name, List, 0), !.
prep_dem_section(Name) :-
    print([Name, 'not found, is a dem file consulted?']).

% prep_dem_section('saint_cloud-w').

cleanup_dem :-
    retractall(demElev(_,_,_)),
    retractall(diffElev(_,_,_)).

size_of_dem(L) :-
    findall(N, demElev(_B,_C,N), List),
    length(List, L).

find_elev(Distance, DiffElev, (DX,DE)) :-
    demElev(X0,Y, E0),
    demElev(X1,Y, E1),
    DX is abs(X1 - X0),
    Distance > DX,
    DE is abs(E1 - E0),
    DiffElev > DE.


label_elev(Distance, DiffElev) :-
    X0 in 0..1200,
    Distance in 1..20,
    Y in 0..12,
    X1 #= Distance + X0,
    label([Distance,X0,Y]),
    demElev(X0,Y, E0),
    % X1 is Distance + X0,
    demElev(X1,Y, E1),
    DiffElev is abs(E1 - E0),
    DiffElev < 21.
    % labeling([], [Distance,X0,Y]).

store_elev_diff(Distance, DiffElev) :-
    diffElev(Distance, DiffElev, N),
    retract(diffElev(Distance,DiffElev,N)),
    M is N + 1,
    assert(diffElev(Distance,DiffElev,M)), !.
store_elev_diff(Distance, DiffElev) :-
    assert(diffElev(Distance,DiffElev,1)).



count_elev(Distance) :-
    % X0 in 0..1200,
    % Y in 0..1200,
    demElev(X0,Y, E0),
    X1 is Distance + X0,
    demElev(X1,Y, E1),
    DiffElev is abs(E1 - E0),
    store_elev_diff(Distance, DiffElev).

    % label([X0,Y]),


find_elevs(Distance, DiffElev, N) :-
    findall(Pair, find_elev(Distance, DiffElev,Pair), Elevs),
    length(Elevs, N).

label_elevs(N) :-
    findall([Dist,Elev], label_elev(Dist, Elev), Elevs),
    length(Elevs, N),
    context_file_reading:parse(Elevs, [], X, [], Y),
    context_r_demo:rhist2d(X,Y).

count_elevs :-
    Distance in 1..40,
    label([Distance]),
    aggregate_all(count, count_elev(Distance), N),
    print(user_error, [Distance, N]).

save_file_json :-
    dem(Name,_),
    findall([A,B,C],
	    diffElev(A,B,C),L),
    concat(Name, '.json', File),
    tell(File),
    write(L),
    told.

write_triple([X,Y,Z]) :-
    write(X), write(','),
    write(Y), write(','),
    write(Z), write('\n').

save_file_csv :-
    dem(Name,_),
    findall([A,B,C],
	    diffElev(A,B,C),L),
    concat(Name, '.csv', File),
    tell(File),
    write('X,Y,Z\nl'),
    maplist(write_triple, L),
    told.

grid_values_y(X) :-
    Y in 0..20,
    label([Y]),
    diffElev(X,Y,Z),
    write(Z), write(','),
    (	Y = 20,
	write('\n');
    true).

grid_values_x :-
    X in 1..40,
    label([X]),
    grid_values_y(X).


grid_values_y(Name, X) :-
    Y in 0..20,
    label([Y]),
    diffElev(Name, X,Y,Z),
    write(Z), write(','),
    (	Y = 20,
	write('\n');
    true).

grid_values_x(Name) :-
    X in 0..40,
    label([X]),
    grid_values_y(Name, X).


save_file_grid(N) :-
    dem(Name,_),
    concat(Name, '.xls', File),
    tell(File),
    aggregate_all(count, grid_values_x, N),
    told.

calc_diff(D0, D1, L) :-
    diffElev(1, 0, C0),
    diffElev(2, 0, C1),
    D0 is (1201*1201/C0)^2/4/pi,
    D1 is (1201*1201/C1)^2/4/pi/2,
    diffElev(1, 1, C2),
    L is -log(C2/C1)/D0.

save_file_grid(N, Name) :-
    concat(Name, '.xls', File),
    tell(File),
    aggregate_all(count, grid_values_x(Name), N),
    told.

% logBase10(X,Y) :- Y is log10(X).

plot_ac :-
    findall([X,Y,Z],
	    (	diffElev('saint_cloud-w',X,Y,Z),
	        Z > 0
	    ),
	    L),
    context_r_demo:xyz(L,[_|A],[_|B],[_|C]),
    ZZ mapdot log10 ~> C,
    context_r_demo:rcontour(A,B,ZZ).

% TESTING
%


test_all_elevs(N) :-
    aggregate_all(count, count_elevs, N) .


test_ll_to_utm :-
    degToRad(-110.49716734332081,Lon),
    degToRad(44.63167101891438,Lat),
    latLonToUTMXY(Lat, Lon, 12, [X,Y]),
    format(atom(S), 'X = ~f , Y = ~f', [X,Y]),
    print(S), nl,
    print('X = 539884          Y = 4942158'), nl.

test_utm_to_ll :-
    utmXYToLatLon(539884.0, 4942158.0, 12, false, [Lat,Lon]),
    radToDeg(Lat, Latitude),
    radToDeg(Lon, Longitude),
    format(atom(S), 'Lat = ~f , Lon = ~f', [Latitude,Longitude]),
    print(S), nl,
    print('Lat = 44.63167101891438  Lon = -110.49716734332081'), nl.

:- dynamic
	profile_elev/1.


test_google(L,Length,Elev) :-
    E1 = 291935,
    N1 = 4906053,
    E2 = 280618,
    N2 = 4914826,
    Distance is sqrt((E1-E2)^2+(N1-N2)^2),
%    DE is (E1-E2)/L,
%    DN is (N2-N1)/L,
%    EP range [E2,E1]/DE,
%    reverse(EP, E),
%    N range [N1,N2]/DN,
%    length(E,Length),
%    length(N,Length), !,
    % N is L + 1,
    get_elevation_path([[E1,E2],[N1,N2]], 15, false, L, Elev), !,
    length(Elev, Length),
    Delta is Distance/Length,
    X range [Delta,Distance]/Delta,
    length(X, Length),
    asserta(profile_elev(Elev)),
    context_r_demo:rplot(X,Elev).


test_google_dunes :-
    N1 = 41.6,
    N2 = 42.0,
    Lon = -101.6,
    L = 500,
    Distance is N2-N1,
    get_elevation_path([[N1,N2],[Lon,Lon]], L, Elev), !,
    length(Elev, Length),
    Delta is Distance/Length,
    X range [Delta,Distance]/Delta,
    length(X, Length),
    % asserta(profile_elev(Elev)),
    context_r_demo:rplot(X,Elev).

test_google_bluffs :-
    N1 = 41.6,
    N2 = 42.0,
    Lon = -102.1,
    L = 500,
    Distance is N2-N1,
    get_elevation_path([[N1,N2],[Lon,Lon]], L, Elev), !,
    length(Elev, Length),
    Delta is Distance/Length,
    X range [Delta,Distance]/Delta,
    length(X, Length),
    % asserta(profile_elev(Elev)),
    context_r_demo:rplot(X,Elev).

test_google_ravines :-
    L1 = -102.4,
    L2 = -101.8,
    Lat = 35.5,
    L = 500,
    Distance is L2-L1,
    get_elevation_path([[Lat,Lat],[L1,L2]], L, Elev), !,
    length(Elev, Length),
    Delta is Distance/Length,
    X range [Delta,Distance]/Delta,
    length(X, Length),
    % asserta(profile_elev(Elev)),
    context_r_demo:rplot(X,Elev).





