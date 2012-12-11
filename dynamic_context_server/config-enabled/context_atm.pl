:- module(context_atm, [atmPartialPressureWater/3,
			atmTemperatureForPartialPressureWater/3,
			atmPressureDryAdiabatic/3,
			atmAltitudeAtPressureDryAdiabatic/3,
			boilingPointH20/3,
                        idealDensity/5,
                        idealPressure/5,
                        idealTemperature/5,
                        plancks_law/4,
                        plancks_law_wavenumber/4
                        % plancks_law_frequency/4
		       ]).

:- use_module(context_math).
:- use_module(context_units).

:- context:register(context_atm:navigate).
:- context:register(context_atm:plot).


%  decorate(X, option([value(X)],[X])).

navigate(Request) :-
   rdf_units(ent:standardAtmosphere, ent:description, Description),
   /*
   t_units(Tlist),
   maplist(decorate, Tlist, Tunits),
   p_units(Plist),
   maplist(decorate, Plist, Punits),
   */
   collect_unit_options(ent:pressure, Punits),
   collect_unit_options(ent:temperature, Tunits),
   collect_unit_options(ent:density, Dunits),

   reply_html_page(cliopatria(default),
                   [title('Standard Atmosphere')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Standard Atmosphere Specifications'),
                      p(Description),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  select([name('p_units')], Punits),
			  select([name('d_units')], Dunits),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [['Vapor Pressure', 'vaporPressure'],
                                          ['Temperature vs Vapor Pressure', 'vaporTemperature'],
                                          ['Ideal Density vs Pressure', 'idealDensity'],
                                          ['Ideal Pressure vs Temperature', 'idealPressure']
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('lin')]),
			  input([type('submit'), name(kind), value('log')])
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
                                         ))
     ]
		  ).


plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
                              t_units(TUnits, []),
                              p_units(PUnits, []),
                              d_units(DUnits, []),
                              evaluate(Characteristic, [])]),
    (
       Characteristic = vaporPressure ->
         T range [1.0, 100.0]/1.0*TUnits,
	 P mapdot atmPartialPressureWater(PUnits) ~> T,
	 % context_units:convert_apply(P, _*PUnits, [], P1), %print(user_error,P1),
	 Data tuple T + P,
         X = 'Temperature',
         Y = 'Partial Pressure',
         XUnits = TUnits,
         YUnits = PUnits
     ;
       Characteristic = vaporTemperature ->
         P range [0.01, 1.0]/0.01*PUnits,
	 T mapdot atmTemperatureForPartialPressureWater(TUnits) ~> P,
	 % context_units:convert_apply(T, _*TUnits, [], T1), %print(user_error,P1),
	 Data tuple P + T,
         X = 'Partial Pressure',
         Y = 'Temperature',
         XUnits = PUnits,
         YUnits = TUnits
      ;
       Characteristic = idealDensity ->
         term_to_atom(DUnit, DUnits),
         P range [0.01, 1.0]/0.01*PUnits,
	 D mapdot idealDensity(DUnit,28*au,30.0*c) ~> P,
	 Data tuple P + D,
         X = 'Pressure',
         Y = 'Density',
         XUnits = PUnits,
         YUnits = DUnits
      ;
       Characteristic = idealPressure ->
         T range [0.01, 100.0]/1.0*TUnits,
	 P mapdot idealPressure(PUnits,28*au,1*kg/m^3) ~> T,
	 Data tuple T + P,
         X = 'Temperature',
         Y = 'Pressure',
         XUnits = TUnits,
         YUnits = PUnits

    ),
    reply_html_page([title('AR 70-38'),
                     \(con_text:style)],
                    [
                            \(context_graphing:dygraph_native(Kind, [X, Y],
							      [X,XUnits], [Y, YUnits],
                                                              'STP Curve', Data))
                    ]
		  ).


atmPartialPressureWater(T*Temperature, P*Pressure) :-
   context_units:convert(T*Temperature, T1*c, T1),
   % var(P), % second term is var
   rdf_units(ent:standardAtmosphere, ent:temperature, T0*k),
   rdf_units(ent:physicalConstants, ent:gasConstant, R*cal/k/mol),
   rdf_units(ent:standardAtmosphere, ent:pressure, P0*atm),
   InvT is 1.0/(T1 + T0),
   rdf_units(ent:water, ent:deltaHvap, DeltaHvap*cal),
   rdf_units(ent:water, ent:deltaSvap, DeltaSvap*cal/k),
   Exp is DeltaSvap/R - DeltaHvap/R * InvT,
   P1 is P0*exp(Exp),
   context_units:convert(P1*atm, P*Pressure, P), !.
atmPartialPressureWater(PressureUnits, T*Temperature, P) :-
	atmPartialPressureWater(T*Temperature, P*PressureUnits).

atmTemperatureForPartialPressureWater(P*Pressure, T*Temperature) :-
   context_units:convert(P*Pressure, P1*atm, P1),
   % var(T),
   rdf_units(ent:standardAtmosphere, ent:temperature, T0*k),
   rdf_units(ent:physicalConstants, ent:gasConstant, R*cal/k/mol),
   rdf_units(ent:standardAtmosphere, ent:pressure, P0*atm),
   rdf_units(ent:water, ent:deltaHvap, DeltaHvap*cal),
   rdf_units(ent:water, ent:deltaSvap, DeltaSvap*cal/k),
   T1 is DeltaHvap/(DeltaSvap - R*log(P1/P0)) - T0,
   context_units:convert(T1*c, T*Temperature, T), !.
atmTemperatureForPartialPressureWater(TemperatureUnits, P*Pressure, T) :-
	atmTemperatureForPartialPressureWater(P*Pressure, T*TemperatureUnits).

atmPressureDryAdiabatic(Alt*Altitude, P*Pressure) :-
   context_units:convert(Alt*Altitude, Alt1*km, Alt1),
   % var(P),
   rdf_units(ent:standardAtmosphere, ent:pressure, P0*atm),
   rdf_units(ent:standardAtmosphere, ent:dryAdiabaticPressureHead, Ph*km),
   rdf_units(ent:standardAtmosphere, ent:seaLevel, SeaLevel*km),
   Alt0 is Alt1 + SeaLevel,
   P1 is P0*exp(Alt0/Ph),
   context_units:convert(P1*atm, P*Pressure, P), !.
atmPressureDryAdiabatic(PressureUnits, Alt*Altitude, P) :-
	atmPressureDryAdiabatic(Alt*Altitude, P*PressureUnits).

atmAltitudeAtPressureDryAdiabatic(P*Pressure, Alt*Altitude) :-
   context_units:convert(P*Pressure, P1*atm, P1),
   % var(Alt),
   rdf_units(ent:standardAtmosphere, ent:pressure, P0*atm),
   rdf_units(ent:standardAtmosphere, ent:dryAdiabaticPressureHead, Ph*km),
   rdf_units(ent:standardAtmosphere, ent:seaLevel, SeaLevel*km),
   Alt1 is Ph*log(P1/P0) + SeaLevel, !,
   context_units:convert(Alt1*km, Alt*Altitude, Alt), !.
atmAltitudeAtPressureDryAdiabatic(AltitudeUnits, P*Pressure, Alt) :-
	atmAltitudeAtPressureDryAdiabatic(P*Pressure, Alt*AltitudeUnits).


boilingPointH20(Alt*Altitude, T*Temperature) :-
   context_units:convert(Alt*Altitude, Alt1*km, Alt1),
   % var(T),
   atmPressureDryAdiabatic(Alt1*km, P*atm),
   atmTemperatureForPartialPressureWater(P*atm, T1*c),
   context_units:convert(T1*c, T*Temperature, T), !.
boilingPointH20(TemperatureUnits, Alt*Altitude, T) :-
	boilingPointH20(Alt*Altitude, T*TemperatureUnits).



idealPressure(MW*au, D*Mass/Vol^3, T*Temperature, P*Pressure) :-
   context_units:convert(T*Temperature, T1*k, T1),
   context_units:convert(D*Mass/Vol^3, D1*g/m^3, D1),
   rdf_units(ent:physicalConstants, ent:gasConstant, R*j/k/mol),
   P1 is D1*T1*R/MW,
   context_units:convert(P1*pa, P*Pressure, P), !.
idealPressure(Pressure, MW*au, D*Mass/Vol^3, T*Temperature, P) :-
    idealPressure(MW*au, D*Mass/Vol^3, T*Temperature, P*Pressure).


idealTemperature(MW*au, D*Mass/Vol^3, P*Pressure, T*Temperature) :-
   context_units:convert(P*Pressure, P1*pa, P1),
   context_units:convert(D*Mass/Vol^3, D1*g/m^3, D1),
   rdf_units(ent:physicalConstants, ent:gasConstant, R*j/k/mol),
   T1 is P1/D1/R*MW,
   context_units:convert(T1*k, T*Temperature, T),!.
idealTemperature(Temperature, D*Mass/Vol^3, MW*au, P*Pressure, T*Temperature) :-
    idealTemperature(MW*au, D*Mass/Vol^3, P*Pressure, T).

idealDensity(MW*au, T*Temperature, P*Pressure, D*Mass/Vol^3) :-
   context_units:convert(T*Temperature, T1*k, T1),
   context_units:convert(P*Pressure, P1*pa, P1),
   rdf_units(ent:physicalConstants, ent:gasConstant, R*j/k/mol),
   D1 is P1/T1/R*MW,
   context_units:convert(D1*g/m^3, D*Mass/Vol^3, D), !.
idealDensity(Mass/Vol^3, MW*au, T*Temperature, P*Pressure, D) :-
    idealDensity(MW*au, T*Temperature, P*Pressure, D*Mass/Vol^3).


plancks_law(T*Temperature, Lambda*Wavelength, P/Wavelength) :-
   context_units:convert(T*Temperature, T1*k, T1),
   context_units:convert(Lambda*Wavelength, WL*m, WL),
   % context_units:convert(1*Wavelength, Scale*m, Scale),
   rdf_units(ent:physicalConstants, ent:planckConstant, H*j*s),
   rdf_units(ent:physicalConstants, ent:speedLight, C*m/s),
   rdf_units(ent:physicalConstants, ent:boltzmannConstant, K*j/k),
   Exp is exp(H*C/(WL*K*T1))-1.0,
   % SpectralEnergyDesnity is 4*pi/C,
   P is 2*H*(C^2)/(WL^5)/Exp, !.
   % context_units:convert(P1/m, P/Wavelength, P), !.
plancks_law(Wavelength, T*Temperature, Lambda*Wavelength, P) :-
   plancks_law(T*Temperature, Lambda*Wavelength, P/Wavelength).

plancks_law_wavenumber(T*Temperature, WN/Wavenumber, P*Wavenumber) :-
   context_units:convert(T*Temperature, T1*k, T1),
   context_units:convert(WN/Wavenumber, S/m, S),
   rdf_units(ent:physicalConstants, ent:planckConstant, H*j*s),
   rdf_units(ent:physicalConstants, ent:speedLight, C*m/s),
   rdf_units(ent:physicalConstants, ent:boltzmannConstant, K*j/k),
   Exp is exp(H*S*C/(K*T1))-1.0,
   P is 2*H*(C^2)*(S^3)/Exp.
   % context_units:convert(P1/m, P/Wavenumber, P), !.
plancks_law_wavenumber(Wavenumber, T*Temperature, WN*Wavenumber, P) :-
   plancks_law_wavenumber(T*Temperature, WN/Wavenumber, P*Wavenumber).

/*
plancks_law_frequency(T*Temperature, WN*Wavenumber, P/Wavenumber) :-
   context_units:convert(T*Temperature, T1*k, T1),
   context_units:convert(WN*Wavenumber, S*m, S),
   rdf_units(ent:physicalConstants, ent:planckConstant, H*j*s),
   rdf_units(ent:physicalConstants, ent:speedLight, C*m/s),
   rdf_units(ent:physicalConstants, ent:boltzmannConstant, K*j/k),
   Exp is exp(H*S*C/(K*T1))-1.0,
   P1 is 2*H*C^2*S^3/Exp,
   context_units:convert(P1/m, P/Wavenumber, P), !.
plancks_law_frequency(Wavenumber, T*Temperature, WN*Wavenumber, P) :-
   plancks_law_frequency(T*Temperature, WN*Wavenumber, P/Wavenumber).
*/


generate_pressure_curve(T,P) :-
   T range [0.0, 100.0]/10.0,
   P mapdot atmPartialPressureWater ~> T.

% Tests
%
% :- context_atm:atmPartialPressureWater(150.0*c, P*atm) :- P>4.0,
% P<5.0.
