:- module(context_psd_workflow_json, []).

/** <module> Context model workflow for PSD
    * classes
    * format
    * characteristic
*/

:- context:register(context_psd_workflow_json:read_tops).
:- context:register(context_psd_workflow_json:psd_files).
:- context:register(context_psd_workflow_json:psd_results).
:- context:register(context_psd_workflow_json:psd_index).

%%   path_to_tops_psd_files(-Path)
%
%    Path to JSON files
path_to_tops_psd_files('../../../../Models/Land/terrains/grades_slopes/data/WaveNumberSpectra/LogSpaced/').

%%   psd_name_file_pair(+Name, -FilePath)
%
%    File retrieve
psd_name_file_pair(Name, FilePath) :-
    rdf(Set, ent:type, literal(terrain_psd)),
    rdf(Set, ent:'PSD_JSON_file', literal(File)),
    rdf(Set, ent:name, literal(Name)),
    path_to_tops_psd_files(Path),
    atom_concat(Path,File,FilePath).

%%   psd_index(+Request)
%
%    Generate index based on JSON files
psd_index(_) :-
   con_text:reply_frameset_page(
     [ frameset([cols('20%,80%'),border(1)],
                [frame([src(psd_files),name(files)]),
                 frame([src(psd_results),name(results)])
                ]
                )]
     ).

%%   get_all_tops_psd_files(+List)
%
%    Find all TOPS PSD files
get_all_tops_psd_files(List) :-
    findall(option([value(FilePath)],[Name]), psd_name_file_pair(Name, FilePath), List).

%%   psd_files(+Request)
%
%    PSD file reaad
psd_files(_) :-
   get_all_tops_psd_files(List),
   reply_html_page(
     title('PSD files'),
     [p(b(u('Available'))),
      form([action('read_tops'), target('results')],
           [
            select([name('file_name')], List),
            br([]),
            \(con_text:radio_box_input_two('plot_scaling',
					   ['log base 10', 'log'],
					   ['linear', 'lin'])),
            input([type('submit'), value('Select PSD Data')])
           ])
      ]
     ).

%%   psd_results(+Request)
%
%    PSD results page
psd_results(_) :-
   reply_html_page(
     title('results'),
     [p(b(u('Results'))) ]
     ).

%%   plot_xy(+Input, +Data)
%
%    Plot XY
plot_xy([], S, Result) :-
   context_demos:json_complete(x(_,[]),y(_,[],_), S, Result).
plot_xy([[X,XU,Y,YU,ID]|Rest], S, R) :-
   context_demos:json_concat(x(XU,X),y(YU,Y,ID),S,Result),
   plot_xy(Rest, Result, R).

plot_xy(Input, Data) :-
   plot_xy(Input, '', Data).

%%   create_data_list(+X,+Y,+XU,+YU,+ID,+Input,-Output)
%
%    Massage the data before plotting
create_data_list([],[],_,_,_,F,F).
% embedded data with one value list
create_data_list([[X]|XR],[[Y]|YR],XU,YU,ID,Input,Output) :-
    create_data_list(XR,YR,XU,YU,ID,[[X,XU,Y,YU,ID]|Input],Output).
% embedded data with single values
create_data_list([X|XR],[Y|YR],XU,YU,ID,Input,Output) :-
    create_data_list(XR,YR,XU,YU,ID,[[X,XU,Y,YU,ID]|Input],Output).


%%   read_psd_json(+FN, +Tuple, +Data, -S,-P)
%
%    Read the PSD from JSON
read_psd_json(FN, [File,Name,Title,Date,DX,UX,DY,UY], Data, S,P) :-
   (
   open(FN, read, F),
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Y' = DY,
                                         'DataUnits_Y' = UY,
                                         'Datalabel_Z' = _DZ,
                                         'DataUnits_Z' = _UZ,
                                         'Course_Length' = _CL,
                                         'Length_Unit' = _LU,
                                         'Grade' = _G,
                                         'Grade_Unit' = _GU,
                                         'Surface_Material' = _SM,
                                         'Data_X' = X,
                                         'Data_Y' = Y,
                                         'Data_Z' = _Z
                                        ])
                                 ])
                     ])),
    close(F)
    ;
  open(FN, read, F),
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Y' = DY,
                                         'DataUnits_Y' = UY,
                                         'Data_X' = X,
                                         'Data_Y' = Y
                                        ])
                                 ])
                     ])),
    close(F)
   ),
   context_graphing:axis_label(DX, UX, S),
   context_graphing:axis_label(DY, UY, P),
   create_data_list(X,Y,S,P,'x',[],Array),
   plot_xy(Array,Data).

%%   read_tops(+Request)
%
%    Read a TOPS PSD 
read_tops(Request) :-
   http_parameters(Request, [plot_scaling(LL),
                             file_name(FN)
                             ],
                             [attribute_declarations(param)]),
   read_psd_json(FN, [File,Name,Title,Date,DX,UX,DY,UY], V, S, P),
   % assert FN = File
   reply_html_page([title(Title),
                    \(context_graphing:scatter_script_load) ],
                   [
                    dl([b(dt('Name')),dd(Name),
                        b(dt('File')),dd(File),
                        b(dt('Title')),dd(Title),
                        b(dt('Date')),dd(Date),
                        b(dt('X')),dd(DX),
                        b(dt('X units')),dd(UX),
                        b(dt('Y')),dd(DY),
                        b(dt('Y units')),dd(UY),
                        b(dt('log or linear scale')),dd(LL)]),
                    \(context_graphing:scatter_plot(LL, S, P ,V))
                   ]).













