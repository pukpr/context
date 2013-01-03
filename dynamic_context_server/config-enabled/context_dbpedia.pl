:- module(context_dbpedia, []).

/** <module> Context dbpedia utilities
    * Hides SPARQL interface
    *
*/


:- use_module(library(semweb/sparql_client)).

%%   get_uri(+Location, -URI)
%
%    Get dbpedia URI location
get_uri(Location, URI) :-
    (
       rdf_global_id(dbpedia:_, Location) ->
       URI = Location
    ;
       rdf_global_id(dbpedia:Location, URI)
    ).

%%   get_lat_lon(+Location, -Lat, -Lon)
%
%    Get latlon for location
get_lat_lon(Location, Lat, Lon) :-
    get_uri(Location, URI),
    Format = 'select * where {<~w> geo:lat ?Lat; geo:long ?Long.}',
    format(atom(Q), Format, [URI]),
    sparql_query(Q, row(C1, C2), [ host('dbpedia.org'), path('/sparql/')] ),
    context:strip_numbers([C1, C2], [], [Lat, Lon]), !.
get_lat_lon(_Location, '?', '?').

%%   get_depiction(+Location, -Image)
%
%    Dbpedia locations have an image
get_depiction(Location, Image) :-
    get_uri(Location, URI),
    Format = 'select * where {<~w> foaf:depiction ?Image.}',
    format(atom(Q), Format, [URI]),
    sparql_query(Q, row(Image), [ host('dbpedia.org'), path('/sparql/')] ).
get_depiction(_, '#').


%%   get_page(+Location, -Page)
%
%    Get a page from dbpedia
get_page(Location, Page) :-
    (
       rdf_global_id(dbpedia:Local, Location) ->
       true
    ;
       Local = Location
    ),
    atom_concat('http://dbpedia.org/page/', Local, Page).
