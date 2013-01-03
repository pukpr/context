:- module(context_csir, []).

/** <module> Rules for capturing terrsin data
    * Used to convert CSV to triple-store
    * Data from CSIR applies
    *
*/

%%   path_to_obstacles_files(-Path)
%
%    Path to obstacle files
path_to_obstacles_files(
    '../../../../Models/Land/terrains/grades_slopes/tools/Unity/RoadExperiments/Assets/Resources/Obstacles/').

%%   get_all_course_files(-List)
%
%    Get all course files
get_all_course_files(List) :-
    findall(Name, rdf_(Name, crg, _), List).

%%   csv_name_file_pair(+Name, +Pred, -FilePath)
%
%    Get CSV version of file
csv_name_file_pair(Name, Pred, FilePath) :-
    rdf_(Name, Pred, File),
    path_to_obstacles_files(Path),
    atom_concat(Path,File,FilePath).

%%   capture(URI) 
%
%    One time capture of data to RDF
capture(URI) :-
   context:prefix(E),
   csv_name_file_pair(URI, crg, FN),
   rdf_(URI, lat, Lat),
   rdf_(URI, lon, Lon),
   rdf_(URI, crg, File),
   findall(Models, rdf_(URI, model, Models), Model_List),
   rdf_(URI, filter, Filter),

   context_file_reading:read_tabbed_csv(FN, X, Z, 2, XX, ZZ),
   context:storeRDF(URI, E, model, Model_List),
   context:storeRDF(URI, E, filter, Filter),
   context:storeRDF(URI, E, src_file, File),
   context:storeRDF(URI, E, units_x, X),
   context:storeRDF(URI, E, units_y, Z),
   context:storeRDF(URI, E, data_x, XX),
   context:storeRDF(URI, E, data_y, ZZ),
   context:storeRDF(URI, E, lat, Lat),
   context:storeRDF(URI, E, lon, Lon),
   context:storeRDF(URI, E, type, terrain_profile),
   context:storeRDF(URI, E, label_x, 'longitudinal distance'),
   context:storeRDF(URI, E, label_y, 'elevation'),
   context:storeRDF(URI, E, course_name, URI),
   context:storeRDF(URI, E, name, URI),
   % context:storeRDF(URI, E, rdf:type, rdfs:'Resource'),
   context:storeRDF(URI, E, date, '1 September 2012'),
   context:storeRDF(URI, E, title, 'CSIR track profile').

%%   generate 
%
%    Generate list of course files
generate :-
    get_all_course_files(List),
    maplist(capture, List).



