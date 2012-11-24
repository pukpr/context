:- module(context_data_lib, []).

/** <module> Context model data files
    * PSD files
    * Obstacle profiles
    * Converted into RDF and stored in triples knowledgebase
*/

directory_location('../../../library/data/').

file_json('10_Inch_Half_Round.JSON').
file_json('12_Degree_Roll_On_Ramp.JSON').
file_json('12_Inch_Half_Round.JSON').
file_json('15_Degree_Roll_On_Ramp.JSON').
file_json('4_Inch_Half_Round.JSON').
file_json('6_Inch_Half_Round.JSON').
file_json('8_Inch_Half_Round.JSON').
file_json('9_Inch_Half_Round.JSON').
file_json('ATC_10percentSoilSlope.JSON').
file_json('ATC_15percentSoilSlope.JSON').
file_json('ATC_1InchBumps.JSON').
file_json('ATC_20percentSoilSlope.JSON').
file_json('ATC_25percentSoilSlope.JSON').
file_json('ATC_30percentSoilSideSlope.JSON').
file_json('ATC_40percentSoilSideSlope.JSON').
file_json('ATC_60percentSoilSlope.JSON').
file_json('ATC_Belgian_Block_10_logspaced.JSON').
file_json('ATC_Belgian_Block_90_logspaced.JSON').
file_json('ATC_Belgian_Block_logspaced.JSON').
file_json('ATC_Churchville_B_10_logspaced.JSON').
file_json('ATC_Churchville_B_90_logspaced.JSON').
file_json('ATC_Churchville_B_logspaced.JSON').
file_json('ATC_Churchville_C_10_logspaced.JSON').
file_json('ATC_Churchville_C_90_logspaced.JSON').
file_json('ATC_Churchville_C_logspaced.JSON').
file_json('ATC_CrosstieCourse.JSON').
file_json('ATC_ElevationStabilizationBump.JSON').
file_json('ATC_MTA10percentAsphaltSlope.JSON').
file_json('ATC_MTA15percentAsphaltSlope.JSON').
file_json('ATC_MTA20percentAsphaltSlope.JSON').
file_json('ATC_MTA30percentConcreteSlope.JSON').
file_json('ATC_MTA40percentConcreteSlope.JSON').
file_json('ATC_MTA45percentConcreteSlope.JSON').
file_json('ATC_MTA50percentConcreteSlope.JSON').
file_json('ATC_MTA5percentAsphaltSlope.JSON').
file_json('ATC_MTA60percentConcreteSlope.JSON').
file_json('ATC_MTA_15DegreeLoadingRamp.JSON').
file_json('ATC_MTA_18InchWall.JSON').
file_json('ATC_MTA_20DegreeLoadingRamp.JSON').
file_json('ATC_MTA_24InchWall.JSON').
file_json('ATC_MTA_2InchWashboard.JSON').
file_json('ATC_MTA_36InchWall.JSON').
file_json('ATC_MTA_3InchSpacedBump.JSON').
file_json('ATC_MTA_42InchWall.JSON').
file_json('ATC_MTA_6InchWashboard.JSON').
file_json('ATC_MTA_ProfileIVCourse.JSON').
file_json('ATC_MTA_RadialWashboard.JSON').
file_json('ATC_MTA_StandardDitch.JSON').
file_json('ATC_MTA_WaveCourse.JSON').
file_json('ATC_Munson_Gravel_10_logspaced.JSON').
file_json('ATC_Munson_Gravel_90_logspaced.JSON').
file_json('ATC_Munson_Gravel_logspaced.JSON').
file_json('ATC_Perryman_1_10_logspaced.JSON').
file_json('ATC_Perryman_1_90_logspaced.JSON').
file_json('ATC_Perryman_1_logspaced.JSON').
file_json('ATC_Perryman_2_10_logspaced.JSON').
file_json('ATC_Perryman_2_90_logspaced.JSON').
file_json('ATC_Perryman_2_logspaced.JSON').
file_json('ATC_Perryman_3_10_logspaced.JSON').
file_json('ATC_Perryman_3_90_logspaced.JSON').
file_json('ATC_Perryman_3_logspaced.JSON').
file_json('ATC_Perryman_A_10_logspaced.JSON').
file_json('ATC_Perryman_A_90_logspaced.JSON').
file_json('ATC_Perryman_A_logspaced.JSON').
file_json('ATC_PotholeCourse.JSON').
file_json('ATC_RRC9Stabilization.JSON').
file_json('Jersey_Barrier_inset.JSON').
file_json('Jersey_Barrier_whole.JSON').
file_json('YTC_11_Inch_Pothole.JSON').
file_json('YTC_12_Inch_Pothole.JSON').
file_json('YTC_4_Inch_Pothole.JSON').
file_json('YTC_6_Inch_Pothole.JSON').
file_json('YTC_9_Inch_Pothole.JSON').
file_json('YTC_Desert_March_logspaced.JSON').
file_json('YTC_KOFA_Level_Gravel_logspaced.JSON').
file_json('YTC_Laguna_Hilly_Trails_logspaced.JSON').
file_json('YTC_Laguna_Level_Trails_East_logspaced.JSON').
file_json('YTC_Laguna_Level_logspaced.JSON').
file_json('YTC_Laguna_Levels_Trails_West_logspaced.JSON').
file_json('YTC_Laguna_Paved_logspaced.JSON').
file_json('YTC_MOUT_9inHalfRounds.JSON').
file_json('YTC_MOUT_Curb.JSON').
file_json('YTC_MOUT_Log.JSON').
file_json('YTC_MOUT_Staircase.JSON').
file_json('YTC_MOUT_Washboard.JSON').
file_json('YTC_Mid-East_Sec_B_logspaced.JSON').
file_json('YTC_Mid-East_Start_logspaced.JSON').
file_json('YTC_Mid-East_WashEnd_logspaced.JSON').
file_json('YTC_MidEast_Sec_A_logspaced.JSON').
file_json('YTC_Patton_Hilly_Gravel_logspaced.JSON').
file_json('YTC_Patton_Hilly_Trails_logspaced.JSON').
file_json('YTC_Patton_Level_Gravel_logspaced.JSON').
file_json('YTC_Patton_Level_Trails_logspaced.JSON').


storeRDF(A, E, B, C) :-
    is_list(C),
    with_output_to(atom(S), write(C)),
    rdf_global_term(E:B, BB),
    not(rdf(A,BB,_)),
    rdf_assert(A,BB,literal(S)), !.
storeRDF(A, E, B, C) :-
    rdf_global_term(E:B, BB),
    not(rdf(A,BB,_)),
    rdf_assert(A,BB,literal(C)), !.
storeRDF(_A, _E, _B, _C).

/*
storeRDF(A, _E, B, C) :-
    is_list(C),
    print(user_error, [skippedList, A,B,'\n']), !.
storeRDF(A, _E, B, C) :-
    print(user_error, [skipped, A,B,C, '\n']).
*/

prefix('ent').

make_name(Name, Ent, E) :-
    prefix(Ent),
    rdf_global_term(Ent:Name, E0),
    uri_normalized(E0, E).

read_json(FN) :-
   open(FN, read, F),
   (
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Y' = DY,
                                         'DataUnits_Y' = UY,
                                         'Datalabel_Z' = DZ,
                                         'DataUnits_Z' = UZ,
                                         'Course_Length' = CL,
                                         'Length_Unit' = LU,
                                         'Grade' = G,
                                         'Grade_Unit' = GU,
                                         'Surface_Material' = SM,
                                         'Data_X' = X,
                                         'Data_Y' = Y,
                                         'Data_Z' = Z
                                        ])
                                 ])
                     ]));
    close(F),
       fail
   ),
    close(F),

    make_name(Name, Ent, E),
    storeRDF(E, Ent, type, obstacle_profile),
    storeRDF(E, Ent, file_path, FN),
    storeRDF(E, Ent, file, File),
    storeRDF(E, Ent, name, Name),
    storeRDF(E, Ent, title, Title),
    storeRDF(E, Ent, date, Date),
    storeRDF(E, Ent, label_x, DX),
    storeRDF(E, Ent, units_x, UX),
    storeRDF(E, Ent, label_y, DY),
    storeRDF(E, Ent, units_y, UY),
    storeRDF(E, Ent, label_z, DZ),
    storeRDF(E, Ent, units_z, UZ),
    storeRDF(E, Ent, course_length, CL),
    storeRDF(E, Ent, length_unit, LU),
    storeRDF(E, Ent, grade, G),
    storeRDF(E, Ent, grade_unit, GU),
    storeRDF(E, Ent, surface_material, SM),
    storeRDF(E, Ent, data_x, X),
    storeRDF(E, Ent, data_y, Y),
    storeRDF(E, Ent, data_z, Z), !.

read_json(FN) :-
   open(FN, read, F),
   (
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Z1' = DY,
                                         'DataUnits_Z1' = UY,
                                         'Datalabel_Z2' = DZ,
                                         'DataUnits_Z2' = UZ,
                                         'Course_Length' = CL,
                                         'Length_Unit' = LU,
                                         'Grade' = G,
                                         'Grade_Unit' = GU,
                                         'Surface_Material' = SM,
                                         'Data_X' = X,
                                         'Data_Z1' = Y,
                                         'Data_Z2' = Z
                                        ])
                                 ])
                     ]));
    close(F),
       fail
   ),
    close(F),

    make_name(Name, Ent, E),
    storeRDF(E, Ent, type, obstacle_profile),
    storeRDF(E, Ent, file_path, FN),
    storeRDF(E, Ent, file, File),
    storeRDF(E, Ent, name, Name),
    storeRDF(E, Ent, title, Title),
    storeRDF(E, Ent, date, Date),
    storeRDF(E, Ent, label_x, DX),
    storeRDF(E, Ent, units_x, UX),
    storeRDF(E, Ent, label_y, DY),
    storeRDF(E, Ent, units_y, UY),
    storeRDF(E, Ent, label_z, DZ),
    storeRDF(E, Ent, units_z, UZ),
    storeRDF(E, Ent, course_length, CL),
    storeRDF(E, Ent, length_unit, LU),
    storeRDF(E, Ent, grade, G),
    storeRDF(E, Ent, grade_unit, GU),
    storeRDF(E, Ent, surface_material, SM),
    storeRDF(E, Ent, data_x, X),
    storeRDF(E, Ent, data_y, Y),
    storeRDF(E, Ent, data_z, Z), !.


read_json(FN) :-
   open(FN, read, F),
   (
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Y' = DY,
                                         'DataUnits_Y' = UY,
                                         'Datalabel_Z' = DZ,
                                         'DataUnits_Z' = UZ,
                                         'Course_Length' = CL,
                                         'Length_Unit' = LU,
                                         'Surface_Material' = SM,
                                         'Data_X' = X,
                                         'Data_Y' = Y,
                                         'Data_Z' = Z
                                        ])
                                 ])
                     ]));
    close(F),
       fail
   ),
    close(F),

    make_name(Name, Ent, E),
    storeRDF(E, Ent, type, obstacle_profile),
    storeRDF(E, Ent, file_path, FN),
    storeRDF(E, Ent, file, File),
    storeRDF(E, Ent, name, Name),
    storeRDF(E, Ent, title, Title),
    storeRDF(E, Ent, date, Date),
    storeRDF(E, Ent, label_x, DX),
    storeRDF(E, Ent, units_x, UX),
    storeRDF(E, Ent, label_y, DY),
    storeRDF(E, Ent, units_y, UY),
    storeRDF(E, Ent, label_z, DZ),
    storeRDF(E, Ent, units_z, UZ),
    storeRDF(E, Ent, course_length, CL),
    storeRDF(E, Ent, length_unit, LU),
    storeRDF(E, Ent, surface_material, SM),
    storeRDF(E, Ent, data_x, X),
    storeRDF(E, Ent, data_y, Y),
    storeRDF(E, Ent, data_z, Z), !.

read_json(FN) :-
   open(FN, read, F),
   (
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Y' = DY,
                                         'DataUnits_Y' = UY,
                                         'Datalabel_Z' = DZ,
                                         'DataUnits_Z' = UZ,
                                         'Course_Length' = CL,
                                         'Length_Unit' = LU,
			                 'Course_Width' = CW,
			                 'Width_Unit' =  WU,
                                         'Surface_Material' = SM,
                                         'Data_X' = X,
                                         'Data_Y' = Y,
                                         'Data_Z' = Z
                                        ]),
% ==================================
                                 'ObstacleData'=json(OD)
                                 ])
                     ]));
    close(F),
       fail
   ),
    close(F),

    make_name(Name, Ent, E),
    storeRDF(E, Ent, type, obstacle_profile),
    storeRDF(E, Ent, file_path, FN),
    storeRDF(E, Ent, file, File),
    storeRDF(E, Ent, name, Name),
    storeRDF(E, Ent, title, Title),
    storeRDF(E, Ent, date, Date),
    storeRDF(E, Ent, label_x, DX),
    storeRDF(E, Ent, units_x, UX),
    storeRDF(E, Ent, label_y, DY),
    storeRDF(E, Ent, units_y, UY),
    storeRDF(E, Ent, label_z, DZ),
    storeRDF(E, Ent, units_z, UZ),
    storeRDF(E, Ent, course_length, CL),
    storeRDF(E, Ent, length_unit, LU),
    storeRDF(E, Ent, course_width, CW),
    storeRDF(E, Ent, width_unit, WU),
    storeRDF(E, Ent, surface_material, SM),
    storeRDF(E, Ent, obstacle_data, OD),
    storeRDF(E, Ent, data_x, X),
    storeRDF(E, Ent, data_y, Y),
    storeRDF(E, Ent, data_z, Z), !.

read_json(FN) :-
   open(FN, read, F),
   (
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Y' = DY,
                                         'DataUnits_Y' = UY,
                                         'Datalabel_Z' = DZ,
                                         'DataUnits_Z' = UZ,
                                         'Course_Length' = CL,
                                         'Length_Unit' = LU,
                                         'Surface_Material' = SM,
                                         'Data_X' = X,
                                         'Data_Y' = Y,
                                         'Data_Z' = Z
                                        ]),
% ==================================
                                 'ObstacleData'=json(OD)
                                 ])
                     ]));
    close(F),
       fail
   ),
    close(F),

    make_name(Name, Ent, E),
    storeRDF(E, Ent, type, obstacle_profile),
    storeRDF(E, Ent, file_path, FN),
    storeRDF(E, Ent, file, File),
    storeRDF(E, Ent, name, Name),
    storeRDF(E, Ent, title, Title),
    storeRDF(E, Ent, date, Date),
    storeRDF(E, Ent, label_x, DX),
    storeRDF(E, Ent, units_x, UX),
    storeRDF(E, Ent, label_y, DY),
    storeRDF(E, Ent, units_y, UY),
    storeRDF(E, Ent, label_z, DZ),
    storeRDF(E, Ent, units_z, UZ),
    storeRDF(E, Ent, course_length, CL),
    storeRDF(E, Ent, length_unit, LU),
    storeRDF(E, Ent, surface_material, SM),
    storeRDF(E, Ent, obstacle_data, OD),
    storeRDF(E, Ent, data_x, X),
    storeRDF(E, Ent, data_y, Y),
    storeRDF(E, Ent, data_z, Z), !.


read_json(FN) :-
   open(FN, read, F),
   (
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Z1' = DY,
                                         'DataUnits_Z1' = UY,
                                         'Datalabel_Z2' = DZ,
                                         'DataUnits_Z2' = UZ,
                                         'Course_Length' = CL,
                                         'Length_Unit' = LU,
			                 'Course_Width' = CW,
			                 'Width_Unit' =  WU,
                                         'Surface_Material' = SM,
                                         'Data_X' = X,
                                         'Data_Z1' = Y,
                                         'Data_Z2' = Z
                                        ])
                                 ])
                     ]));
    close(F),
       fail
   ),
    close(F),

    make_name(Name, Ent, E),
    storeRDF(E, Ent, type, obstacle_profile),
    storeRDF(E, Ent, file_path, FN),
    storeRDF(E, Ent, file, File),
    storeRDF(E, Ent, name, Name),
    storeRDF(E, Ent, title, Title),
    storeRDF(E, Ent, date, Date),
    storeRDF(E, Ent, label_x, DX),
    storeRDF(E, Ent, units_x, UX),
    storeRDF(E, Ent, label_y, DY),
    storeRDF(E, Ent, units_y, UY),
    storeRDF(E, Ent, label_z, DZ),
    storeRDF(E, Ent, units_z, UZ),
    storeRDF(E, Ent, course_length, CL),
    storeRDF(E, Ent, length_unit, LU),
    storeRDF(E, Ent, course_width, CW),
    storeRDF(E, Ent, width_unit, WU),
    storeRDF(E, Ent, surface_material, SM),
    storeRDF(E, Ent, data_x, X),
    storeRDF(E, Ent, data_y, Y),
    storeRDF(E, Ent, data_z, Z), !.


read_json(FN) :-
   open(FN, read, F),
   (
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Z1' = DY,
                                         'DataUnits_Z1' = UY,
                                         'Datalabel_Z2' = DZ,
                                         'DataUnits_Z2' = UZ,
                                         'Course_Length' = CL,
                                         'Length_Unit' = LU,
                                         'Surface_Material' = SM,
                                         'Data_X' = X,
                                         'Data_Z1' = Y,
                                         'Data_Z2' = Z
                                        ])
                                 ])
                     ]));
    close(F),
       fail
   ),
    close(F),

    make_name(Name, Ent, E),
    storeRDF(E, Ent, type, obstacle_profile),
    storeRDF(E, Ent, file_path, FN),
    storeRDF(E, Ent, file, File),
    storeRDF(E, Ent, name, Name),
    storeRDF(E, Ent, title, Title),
    storeRDF(E, Ent, date, Date),
    storeRDF(E, Ent, label_x, DX),
    storeRDF(E, Ent, units_x, UX),
    storeRDF(E, Ent, label_y, DY),
    storeRDF(E, Ent, units_y, UY),
    storeRDF(E, Ent, label_z, DZ),
    storeRDF(E, Ent, units_z, UZ),
    storeRDF(E, Ent, course_length, CL),
    storeRDF(E, Ent, length_unit, LU),
    storeRDF(E, Ent, surface_material, SM),
    storeRDF(E, Ent, data_x, X),
    storeRDF(E, Ent, data_y, Y),
    storeRDF(E, Ent, data_z, Z), !.

% atom_json_term

read_json(FN) :-
   open(FN, read, F),
   (
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
                     ]));
    close(F),
       fail
   ),
    close(F),

    make_name(Name, Ent, E),
    storeRDF(E, Ent, type, terrain_psd),
    storeRDF(E, Ent, file_path, FN),
    storeRDF(E, Ent, file, File),
    storeRDF(E, Ent, name, Name),
    storeRDF(E, Ent, title, Title),
    storeRDF(E, Ent, date, Date),
    storeRDF(E, Ent, label_x, DX),
    storeRDF(E, Ent, units_x, UX),
    storeRDF(E, Ent, label_y, DY),
    storeRDF(E, Ent, units_y, UY),
    storeRDF(E, Ent, data_x, X),
    storeRDF(E, Ent, data_y, Y), !.


read_json(FN) :-
   open(FN, read, F),
   (
   json_read(F, json([File=json([name=Name,
                                 'Source_Title'=Title,
                                 'Source_Date'=Date,
                                 'Data'=json([
                                         'Datalabel_X' = DX,
                                         'DataUnits_X' = UX,
                                         'Datalabel_Y' = DY,
                                         'DataUnits_Y' = UY,
			                 'Surface_Material' = Mat,
                                         'Surface_FricCoeff' = FC,
                                         'Data_X' = X,
                                         'Data_Y' = Y
                                        ])
                                 ])
                     ]));
    close(F),
       fail
   ),
    close(F),

    make_name(Name, Ent, E),
    storeRDF(E, Ent, type, terrain_psd),
    storeRDF(E, Ent, file_path, FN),
    storeRDF(E, Ent, file, File),
    storeRDF(E, Ent, name, Name),
    storeRDF(E, Ent, title, Title),
    storeRDF(E, Ent, date, Date),
    storeRDF(E, Ent, label_x, DX),
    storeRDF(E, Ent, units_x, UX),
    storeRDF(E, Ent, label_y, DY),
    storeRDF(E, Ent, units_y, UY),
    storeRDF(E, Ent, surface_material, Mat),
    storeRDF(E, Ent, fc, FC),
    storeRDF(E, Ent, data_x, X),
    storeRDF(E, Ent, data_y, Y), !.

read_json(FN) :-
    print(user_error, [notRead, FN,'\n']), !.

populate_rdf_with_file_data :-
    directory_location(Dir),
    file_json(File),
    atom_concat(Dir, File, FN),
    read_json(FN),
    fail.
populate_rdf_with_file_data :-
    print(user_error, ['finished']).








