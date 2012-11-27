:- module(context_water, [table_density/2,
			   salt_density/3,
			   fresh_density/2
			  %    fxn3/3
			  ]).

/** <module> Water data
    * Fresh water density
    * Salt water density
    * Sea state
*/

:- use_module(context).
:- use_module(context_math).

:- context:register(context_water:navigate).
:- context:register(context_water:chart).
:- context:register(context_water:density_test).
:- context:register(context_water:plot).
:- context:register(context_water:densities).


freshwater_density(
    low = -8.0,
    high = 108.0,
    input_units = c,
    output_units = g/cm^3,
    [0.998120, 0.998398, 0.998650, 0.998877, 0.999080, 0.999259, 0.999417, 0.999553, 0.999669, 0.999765,
     0.9998425,0.9999015,0.9999429,0.9999672,0.9999750,0.9999668,0.9999432,0.9999045,0.9998512,0.9997838,
     0.9997026,0.9996018,0.9995004,0.9993801,0.9992474,0.9991026,0.9989460,0.9987779,0.9985986,0.9984082,
     0.9982071,0.9979955,0.9977735,0.9975415,0.9972995,0.9970479,0.9967867,0.9965162,0.9962365,0.9959478,
     0.9956502,0.9953440,0.9950292,0.9947060,0.9943745,0.9940349,0.9936872,0.9933316,0.9929683,0.9925973,
     0.9922187,0.9918327,0.9914394,0.9910388,0.9906310,0.9902162,0.9897944,0.9893657,0.9889303,0.9884881,
     0.9880393,0.9875839,0.9871220,0.9866537,0.9861791,0.9856982,0.9852111,0.9847178,0.9842185,0.9837132,
     0.9832018,0.9826846,0.9821615,0.9816327,0.9810981,0.9805578,0.9800118,0.9794603,0.9789032,0.9783406,
     0.9777726,0.9771991,0.9766203,0.9760361,0.9754466,0.9748519,0.9742520,0.9736468,0.9730366,0.9724212,
     0.9718007,0.9711752,0.9705446,0.9699091,0.9692686,0.9686232,0.9679729,0.9673177,0.9666576,0.9659927,
     0.9653230,0.9646486,0.9639693,0.9632854,0.9625967,0.9619033,0.9612052,0.9605025,0.9597951,0.9590831,
     0.9583665,0.957662 ,0.956937, 0.956207, 0.955472, 0.954733, 0.953989, 0.953240, 0.952488, 0.941730]).

table_density(Input, Output) :-
     freshwater_density(low=Lo, _Hi, _IU, _OU, T),
     context_lookup:lookup_table(1.0, Lo, T, Input, Output).

rhocalc(T, Rho) :- %  temp dependent density
     Rho is 1000*(1.0-(T+288.9414)/(508929.2*(T+68.12963))*(T-3.9863)^2).

rhoscalc(Rho,Conc,T,R) :- %  temp and conc dependent density
     A is 0.824493 - 0.0040899*T + 0.000076438*T^2-0.00000082467*T^3 + 0.0000000053675*T^4,
     B is -0.005724 + 0.00010227*T - 0.0000016546*T^2,
     R is Rho + A*Conc + B*Conc^(3/2) + 0.00048314*Conc^2.

alg_density(T, Conc,Rhos) :-
    C is Conc/1000,
    rhocalc(T, Rho),
    rhoscalc(Rho,C,T,Rhos).

density_test(Request) :-
    http_parameters(Request, [input(Input, [float]),
			      iunits(IUnits, [default('c')]),
			      ounits(OUnits, [default('g/cm^3')])]),
    downcase_atom(IUnits, IU),
    context_units:convert(Input*IU, To*c, To),
    alg_density(To, 0.0, R),
    downcase_atom(OUnits, OU),
    atom_to_term(OU, M/L^3, _U),
    catch(context_units:convert(R*g/dm^3, Result*M/L^3, Result),
          error(syntax_error(Error)),
          atom_concat(Error, ' not valid unit', Result)
         ),
    reply_html_page(
	[title(water)],
	[p(Result)]
		   ).

chart(_Request) :-
    X range [0.0, 100.0]/0.5,
    Y mapdot table_density ~> X,
    Data tuple X + Y,
    reply_html_page([title('Water Density Plot')],
                   [
                    \(context_graphing:dygraph_native(
                                           false,
                                           ['Temperature', 'Density'],
                                           'Temperature',
                                           'Density',
                                           'Water Density Fresh Water',
                                           Data ))
                   ]).


density_cells([Liquid,Temperature,Density]) :-
    rdfS(UID, ent:liquid, Liquid),
    rdfS(UID, ent:temperature, Temperature),
    rdfS(UID, ent:density, Density).

densities(_Request) :-
    findall(Row, density_cells(Row), Rows),
    reply_html_page([title('Liquid Densities Table'),
                     \(con_text:style)],
                   [
                    \(con_text:table_multiple_entries(
                          [['Liquid','Temperature','Density']],
                          Rows))
                   ]).

navigate(Request) :-
   collect_unit_options(ent:mass, Munits),
   collect_unit_options(ent:volume, Vunits),
   collect_unit_options(ent:temperature, Tunits),
   context_file_reading:find('stability curve calculations.xlsx', Stability_File),
   % context_file_reading:find('density.xlsx', Stability_File),
   reply_html_page(cliopatria(default),
                   [title('Buoyancy calculation')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Buoyancy calculation'),
                      p('Compare immersed volume and mass to water density'),
                      form([action(plot), target(target_iframe)],
			 [
			  input([type('text'),
				 name('mass'),
				 size(6),
				 value('3000')]),
			  select([name('m_units')], Munits),
			  input([type('text'),
				 name('volume'),
				 size(6),
				 value('3')]),
			  select([name('v_units')], Vunits),
			  select([name('t_units')], Tunits),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Salt water', 'salt'],
                                          ['Fresh water', 'fresh']
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('lin')]),
			  input([type('submit'), name(kind), value('log')])
			 ]
                          ),
                      br([]),
                      \(con_text:button_link('Liquid Densities', 'densities', 'render')),
                      \(con_text:button_link('Download Stability Spreadsheet',
                                             '/context_file_reading/download',
                                             render,
                                             [[uri, Stability_File],
                                              [type, 'application/excel']])),
                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).


salt_density(Parameter,X*_Units,Y) :-
   alg_density(X, Parameter, Y).
fresh_density(X*_Units,Y) :-
   table_density(X, Y).


plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      mass(Mass, [number]),
			      volume(Volume, [number]),
                              m_units(MUnits, []),
                              v_units(VUnits, []),
                              v_units(TUnits, []),
                              evaluate(Characteristic, [default(salt)])]),

    Density is Mass/Volume,
    H range [0.0, 100.0]/1.0*TUnits,
    (
       Characteristic = salt ->
	 Z mapdot salt_density(0.0) ~> H
     ;
       Characteristic = fresh ->
	 Z mapdot fresh_density ~> H

    ),
    DC mapdot Density ~> H,
    Data tuple H + Z + DC,
    X = 'Temperature',
    Y1 = 'Water Density',
    Y2 = 'Object Density',
    XUnits = 'c',
    format(atom(YUnits), '~w/~w', [MUnits,VUnits]),
    reply_html_page([title('Buoyancy'),
                     \(con_text:style)],
                    [
		     \(context_graphing:dygraph_native(Kind, [X, Y1, Y2],
						       [X,XUnits], ['density',YUnits],
						       'DEnsity', Data))
                    ]
		  ).
