:- module(context_physical, [
			      rdf_units/3
			    ]).

/** <module> Categorization of common physical constants
    * Water
    * Atmosphere
    * Physical properties and constants
    * Solar
    * Dimensional Units
*/

:- use_module(context_math).

:- context:register(context_physical:navigate).


/*
% rdf_units(Subj, Pred, Obj) :-
%    rdfS(Subj, Pred, Obj).
*/
%%   rdf_units(+Sub, +Pred, +Obj)
%
%    Units as triples
rdf_units(Sub, Pred, Obj) :-
    rdf_global_term(ent:Sub, Subject),
    rdf_global_term(ent:Pred, Predicate),
    rdfS(Subject, Predicate, T),
/*
    (
       rdf(Subject, Predicate, literal(type(xsd:string, T)))
    ;
       rdf(Subject, Predicate, literal(T))
    ),
*/
    atom_to_term(T, Obj, []).


%%   find_entries(+Name, -List)
%
%    Find entries from physical constant list
find_entries(Name, List) :-
   findall([Text,Val], (rdfS(Class, ent:name, Name),
			rdfS(Class, Attr, Val),
			rdf_global_id(ent:Text, Attr),
			Val \= ''),
			List).

%%   find_units(-List)
%
%    Find relevant Units list
find_units(List) :-
   findall([Text,Val], (rdf(Attr, ent:units, UID),
			rdfS(UID, ent:unit, Val),
		        % rdf_global_id(ent:Text, Attr)
		        rdfS(Attr, ent:name, Text)
		       ),
			List).

%%   navigate(+Request)
%
%    Dynamic page to physical parameters and constants
navigate(Request) :-
   find_entries(standardAtmosphere, Atm),
   find_entries(water, Water),
   find_entries(solar, Solar),
   find_entries(physicalConstants, PhysConst),
   find_units(Units),
   reply_html_page(cliopatria(default),
                   [title('Physical Parameters and Units')],
		    \(con_text:table_with_iframe_target(
                    Request,
                   [
		    h1('Standard Atmosphere properties'),
		   \(con_text:table_entries(Atm)),
		    h1('Water properties'),
		   \(con_text:table_entries(Water)),
		    h1('Solar'),
		   \(con_text:table_entries(Solar)),
		    h1('Physical Constants'),
		   \(con_text:table_entries(PhysConst)),
		    h1('Units suitable for conversion'),
		   \(con_text:table_entries(Units))
		   ]
		  ))).


/*

% Scientific
rdf_old_units(ent:water, comment, 'fresh water properties').
rdf_old_units(ent:water, ent:molecular_weight, 18.015268*au).
rdf_old_units(ent:water, ent:deltaHvap, 9717.1*cal).
rdf_old_units(ent:water, ent:deltaSvap, 26.04*cal/k).

rdf_old_units(ent:standardAtmosphere, ent:description, 'Specification of standard atmosphere').
rdf_old_units(ent:standardAtmosphere, comment, 'standard temperature and pressure (STP)').

rdf_old_units(ent:standardAtmosphere, ent:temperature, 70*f).
rdf_old_units(ent:standardAtmosphere, ent:temperature, 273.15*k).
rdf_old_units(ent:standardAtmosphere, ent:pressure, 1*atm).
rdf_old_units(ent:standardAtmosphere, ent:dryAdiabaticLapseRate, 9.8*c/km).
rdf_old_units(ent:standardAtmosphere, ent:moistAdiabaticLapseRateTypical, 5.0*c/km).
rdf_old_units(ent:standardAtmosphere, ent:seaLevel, 0.0*km).
rdf_old_units(ent:standardAtmosphere, ent:dryAdiabaticPressureHead, -0.116*km).
% rdf_old_units(ent:standardAtmosphere, ent:troposphericLimit, 0.1*atm).
rdf_old_units(ent:standardAtmosphere, ent:molecularWeight, 28.966*au). %
rdf_old_units(ent:standardAtmosphere, ent:specificHeatRatio, 1.4).
rdf_old_units(ent:standardAtmosphere, ent:specificGasConstantDryAir, 287*j/kg/k).
rdf_old_units(ent:standardAtmosphere, ent:specificGasConstantWaterVapor, 462*j/kg/k).

rdf_old_units(ent:solar, comment, 'average sunshine').
rdf_old_units(ent:solar, ent:averageSolarInsolation, 1366*w/m^2).
rdf_old_units(ent:solar, ent:averageAlbedo, 0.31).

rdf_old_units(ent:physicalConstants, comment, 'commonly used physical constants').
rdf_old_units(ent:physicalConstants, ent:gasConstant, 1.9858775*cal/k/mol).
rdf_old_units(ent:physicalConstants, ent:gasConstant, 8.314472*j/k/mol).
rdf_old_units(ent:physicalConstants, ent:atomicUnit, 1.66053886e-24*g).
rdf_old_units(ent:physicalConstants, ent:avogadrosNumber, 6.02214129e23*n).
rdf_old_units(ent:physicalConstants, ent:stefanBoltzmann, 5.670373e-8*w/m^2/k^4).
rdf_old_units(ent:physicalConstants, ent:gravity, 9.80665*m/s^2).
rdf_old_units(ent:physicalConstants, ent:gravity, 32.174049*ft/s^2).
rdf_old_units(ent:physicalConstants, ent:boltzmannConstant, 1.38065e-23*j/k).
rdf_old_units(ent:physicalConstants, ent:electricalCharge, 1.60218e-19*coulombs).
rdf_old_units(ent:physicalConstants, ent:planckConstant, 6.62607e-34*j*s).
rdf_old_units(ent:physicalConstants, ent:faradaysConstant, 96485.3383*coulombs/mol).
rdf_old_units(ent:physicalConstants, ent:dielectricConstant, 8.854187e-12*fd/m).
rdf_old_units(ent:physicalConstants, ent:speedLight, 3.0e8*m/s).


%  ---- Units symbology
%
rdf_old_units(ent:'All Units', ent:units, ent:'units category').
rdf_old_units(ent:'units category', ent:unit, 'Symbolic Notation').

% length units
rdf_old_units(ent:length, ent:units, ent:micron).
rdf_old_units(ent:micron, ent:unit, 'micron').
rdf_old_units(ent:micron, ent:description, 'microns').

rdf_old_units(ent:length, ent:units, ent:nm).
rdf_old_units(ent:nm, ent:unit, 'nm').
rdf_old_units(ent:nm, ent:description, 'nanometers').

rdf_old_units(ent:length, ent:units, ent:mm).
rdf_old_units(ent:mm, ent:unit, 'mm').
rdf_old_units(ent:mm, ent:description, 'millimeters').

rdf_old_units(ent:length, ent:units, ent:cm).
rdf_old_units(ent:cm, ent:unit, 'cm').
rdf_old_units(ent:cm, ent:description, 'centimeters').

rdf_old_units(ent:length, ent:units, ent:inch).
rdf_old_units(ent:inch, ent:unit, 'in').
rdf_old_units(ent:inch, ent:description, 'inches').

rdf_old_units(ent:length, ent:units, ent:mil).
rdf_old_units(ent:mil, ent:unit, 'mil').
rdf_old_units(ent:mil, ent:description, 'mils').

rdf_old_units(ent:length, ent:units, ent:meters).
rdf_old_units(ent:meters, ent:unit, 'm').
rdf_old_units(ent:meters, ent:description, 'meters').

rdf_old_units(ent:length, ent:units, ent:ft).
rdf_old_units(ent:ft, ent:unit, 'ft').
rdf_old_units(ent:ft, ent:description, 'feet').

rdf_old_units(ent:length, ent:units, ent:yards).
rdf_old_units(ent:yards, ent:unit, 'yd').
rdf_old_units(ent:yards, ent:description, 'yards').

rdf_old_units(ent:length, ent:units, ent:kft).
rdf_old_units(ent:kft, ent:unit, 'kft').
rdf_old_units(ent:kft, ent:description, 'thousand feet').

rdf_old_units(ent:length, ent:units, ent:km).
rdf_old_units(ent:km, ent:unit, 'km').
rdf_old_units(ent:km, ent:description, 'kilometer').



% time units
rdf_old_units(ent:time, ent:units, ent:sec).
rdf_old_units(ent:sec, ent:unit, 's').
rdf_old_units(ent:sec, ent:description, 'seconds').

rdf_old_units(ent:time, ent:units, ent:min).
rdf_old_units(ent:min, ent:unit, 'min').
rdf_old_units(ent:min, ent:description, 'minutes').

rdf_old_units(ent:time, ent:units, ent:hour).
rdf_old_units(ent:hour, ent:unit, 'hr').
rdf_old_units(ent:hour, ent:description, 'hours').

rdf_old_units(ent:time, ent:units, ent:day).
rdf_old_units(ent:day, ent:unit, 'day').
rdf_old_units(ent:day, ent:description, 'days').

rdf_old_units(ent:time, ent:units, ent:year).
rdf_old_units(ent:year, ent:unit, 'yr').
rdf_old_units(ent:year, ent:description, 'years').

rdf_old_units(ent:time, ent:units, ent:decade).
rdf_old_units(ent:decade, ent:unit, 'decade').
rdf_old_units(ent:decade, ent:description, 'decades').

rdf_old_units(ent:time, ent:units, ent:mics).
rdf_old_units(ent:mics, ent:unit, 'mics').
rdf_old_units(ent:mics, ent:description, 'micro-seconds').


% Volume units

rdf_old_units(ent:volume, ent:units, ent:m_3).
rdf_old_units(ent:m_3, ent:unit, 'm^3').
rdf_old_units(ent:m_3, ent:description, 'cubic meter').

rdf_old_units(ent:volume, ent:units, ent:cm_3).
rdf_old_units(ent:cm_3, ent:unit, 'cm^3').
rdf_old_units(ent:cm_3, ent:description, 'cubic centimeter').

% Area units

rdf_old_units(ent:area, ent:units, ent:m_2).
rdf_old_units(ent:m_2, ent:unit, 'm^2').
rdf_old_units(ent:m_2, ent:description, 'square meter').

rdf_old_units(ent:area, ent:units, ent:ft_2).
rdf_old_units(ent:ft_2, ent:unit, 'ft^2').
rdf_old_units(ent:ft_2, ent:description, 'square foot').


% Mass units

rdf_old_units(ent:mass, ent:units, ent:kilogram).
rdf_old_units(ent:kilogram, ent:unit, 'kg').
rdf_old_units(ent:kilogram, ent:description, 'kilogram').

rdf_old_units(ent:mass, ent:units, ent:pound).
rdf_old_units(ent:pound, ent:unit, 'lb').
rdf_old_units(ent:pound, ent:description, 'pound(mass)').

% Pressure units
rdf_old_units(ent:pressure, ent:units, ent:atm).
rdf_old_units(ent:atm, ent:unit, 'atm').
rdf_old_units(ent:atm, ent:description, 'atmospheres').

rdf_old_units(ent:pressure, ent:units, ent:torr).
rdf_old_units(ent:torr, ent:unit, 'torr').
rdf_old_units(ent:torr, ent:description, 'Torr').

rdf_old_units(ent:pressure, ent:units, ent:mtorr).
rdf_old_units(ent:mtorr, ent:unit, 'mtorr').
rdf_old_units(ent:atm, ent:description, 'milli Torr').

rdf_old_units(ent:pressure, ent:units, ent:mpa).
rdf_old_units(ent:mpa, ent:unit, 'mpa').
rdf_old_units(ent:mpa, ent:description, 'milli Pascals').

rdf_old_units(ent:pressure, ent:units, ent:pa).
rdf_old_units(ent:pa, ent:unit, 'pa').
rdf_old_units(ent:pa, ent:description, 'Pascals').

rdf_old_units(ent:pressure, ent:units, ent:psi).
rdf_old_units(ent:psi, ent:unit, 'psi').
rdf_old_units(ent:psi, ent:description, 'pounds/in^2').

rdf_old_units(ent:pressure, ent:units, ent:millibar).
rdf_old_units(ent:millibar, ent:unit, 'millibar').
rdf_old_units(ent:millibar, ent:description, 'milli Bar').

rdf_old_units(ent:pressure, ent:units, ent:bar).
rdf_old_units(ent:bar, ent:unit, 'bar').
rdf_old_units(ent:bar, ent:description, 'Bar').

% Temperature units
rdf_old_units(ent:temperature, ent:units, ent:c).
rdf_old_units(ent:c, ent:unit, 'c').
rdf_old_units(ent:c, ent:description, 'Celsius').

rdf_old_units(ent:temperature, ent:units, ent:f).
rdf_old_units(ent:f, ent:unit, 'f').
rdf_old_units(ent:f, ent:description, 'Fahrenheit').

rdf_old_units(ent:temperature, ent:units, ent:k).
rdf_old_units(ent:k, ent:unit, 'k').
rdf_old_units(ent:k, ent:description, 'Kelvin').

rdf_old_units(ent:temperature, ent:units, ent:r).
rdf_old_units(ent:r, ent:unit, 'r').
rdf_old_units(ent:r, ent:description, 'Rankine').


% p_units([mtorr,torr,atm,mpa,pa,psi,millibar,bar]).
% t_units([c,f,k,r]).

% Density units
rdf_old_units(ent:density, ent:units, ent:g_cm3).
rdf_old_units(ent:g_cm3, ent:unit, 'g/cm^3').
rdf_old_units(ent:g_cm3, ent:description, 'grams/cc').

rdf_old_units(ent:density, ent:units, ent:kg_m3).
rdf_old_units(ent:kg_m3, ent:unit, 'kg/m^3').
rdf_old_units(ent:kg_m3, ent:description, 'kilos/cubic meter').

rdf_old_units(ent:density, ent:units, ent:lb_ft3).
rdf_old_units(ent:lb_ft3, ent:unit, 'lb/ft^3').
rdf_old_units(ent:lb_ft3, ent:description, 'pounds/cubic foot').

rdf_old_units(ent:density, ent:units, ent:oz_in3).
rdf_old_units(ent:oz_in3, ent:unit, 'oz/in^3').
rdf_old_units(ent:oz_in3, ent:description, 'ounces/cubic inch').

rdf_old_units(ent:density, ent:units, ent:tons_km3).
rdf_old_units(ent:tons_km3, ent:unit, 'ton/km^3').
rdf_old_units(ent:tons_km3, ent:description, 'tons/cubic km').

rdf_old_units(ent:density, ent:units, ent:mg_micron3).
rdf_old_units(ent:mg_micron3, ent:unit, 'mg/micron^3').
rdf_old_units(ent:mg_micron3, ent:description, 'mils/micron^3').

rdf_old_units(ent:density, ent:units, ent:kg_liter).
rdf_old_units(ent:kg_liter, ent:unit, 'kg/dm^3').
rdf_old_units(ent:kg_liter, ent:description, 'kilos/liter').

%%   store_as_triples 
%
%    Create as triples
store_as_triples :-
    context:make_name('physical_units_and_constants', Ent, E),
    print(user_error, Ent),
    !,
    rdf_old_units(A,B,C),
    context:storeRDF_to_graph(A, B,C, E),
    fail.


*/
