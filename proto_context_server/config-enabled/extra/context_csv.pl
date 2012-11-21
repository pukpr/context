:- module(context_csv, []).

path_to_obstacles_files(
    '../../../../Models/Land/terrains/grades_slopes/tools/Unity/RoadExperiments/Assets/Resources/Obstacles/').

csv_name_file_pair(Name, Pred, FilePath) :-
    rdf_(Name, Pred, File),
    path_to_obstacles_files(Path),
    atom_concat(Path,File,FilePath).

get_all_obstacle_files(List) :-
    findall(Name, rdf_(Name, csv, _), List).

capture(URI) :-
   context:prefix(E),
   csv_name_file_pair(URI, csv, FN),
   context:storeRDF(URI, E, src_file, FN),
   context:storeRDF(URI, E, name, URI).

generate :-
    get_all_obstacle_files(List),
    maplist(capture, List).



