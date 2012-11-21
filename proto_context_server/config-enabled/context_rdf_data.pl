:- multifile(rdf_/3).
/*
rdf_('/context_select/navigate?category=fine_terrain',  target_iframe, '/html/static_pages/gems/fine_terrain.png').
rdf_('/context_select/navigate?category=gross_terrain', target_iframe, '/html/static_pages/gems/gross_terrain.png').
rdf_('/context_select/navigate?category=wave',          target_iframe, '/html/static_pages/gems/wave_statistics.png').
rdf_('/context_select/navigate?category=wind',          target_iframe, '/html/static_pages/gems/wind_statistics.png').
rdf_('/context_select/navigate?category=clutter',       target_iframe, '/html/static_pages/gems/clutter_modeling.png').
rdf_('/context_select/navigate?category=lakes',         target_iframe, '/html/static_pages/gems/lake_sizes.png').
rdf_('/context_select/navigate?category=particles',     target_iframe, '/html/static_pages/gems/particle_sizes.png').
rdf_('/context_select/navigate?category=thermal',       target_iframe, '/html/static_pages/gems/thermal_dispersion.png').
rdf_('/context_select/navigate?category=rainfall',      target_iframe, '/html/static_pages/gems/rain_statistics.png').
rdf_('/context_select/navigate?category=corrosion',     target_iframe, '/html/static_pages/gems/corrosion_oxidation.png').
rdf_('/app',                                            target_iframe, '/html/static_pages/gems/').
rdf_('/context_require/navigate',                       target_iframe, '/html/images/requirements_flow.gif').
rdf_('/context_ref/navigate',                           target_iframe, '/html/citations.html').
rdf_('/context_browse/browse',                          target_iframe, '/html/browse.html').
rdf_('/context_browse/browse?term=requirements',        target_iframe, '/html/browse.html#requirements').
rdf_('/context_map/search',                             target_iframe, '/html/map.html').
rdf_('/context_resources/navigate',                     target_iframe, '/html/resources.html').
rdf_('/context_workflow/workflows',                     target_iframe, '/html/workflow.html').
rdf_('/context_search/search',                          target_iframe, '/html/search.html').

% rdf_(_, IName, Page) :- atomic_list_concat(['/html/images/',IName,'.gif'], Page).
rdf_(_, IName, Page) :- atomic_list_concat(['/html/',IName,'.html'], Page).
*/

rdf_(fine_terrain, ent:image, 'fine_terrain_icon.png').
rdf_(fine_terrain, ent:description, 'Fine terrain: course and obstacle models').
rdf_(gross_terrain, ent:image, 'gross_terrain_icon.png').
rdf_(gross_terrain, ent:description, 'Topographic terrain: slope and elevation models').
rdf_(wave, ent:image, 'wave_statistics_icon.png').
rdf_(wave, ent:description, 'Wave and sea state models').
rdf_(wind, ent:image, 'wind_statistics_icon.png').
rdf_(wind, ent:description, 'Wind models').
rdf_(clutter, ent:image, 'clutter_modeling_icon.png').
rdf_(clutter, ent:description, 'EMI and clutter models').
rdf_(lakes, ent:image, 'lake_sizes_icon.png').
rdf_(lakes, ent:description, 'Lake, river models').
rdf_(particles, ent:image, 'particle_sizes_icon.png').
rdf_(particles, ent:description, 'Particle and debris models').
rdf_(thermal, ent:image, 'thermal_dispersion_icon.jpg').
rdf_(thermal, ent:description, 'Thermal models').
rdf_(rainfall, ent:image, 'rain_statistics_icon.png').
rdf_(rainfall, ent:description, 'Rainfall, cloud models').
rdf_(corrosion, ent:image, 'corrosion_oxidation_icon.jpg').
rdf_(corrosion, ent:description, 'Corrosion, oxidation models').

rdf_(require, ent:description, 'User specified context requirements.').
rdf_(require, ent:narrative, 'Project-specific requirements provide a means to connect practical applications to information available from an environmental context library. For example, a project requirement that states that a vehicle should be able to operate on terrain of a specific roughness, suggests a link to certain context models available from the server.  The links between the requirements and models are accomplished via semantic and ontological organization of the knowledge. In this case, certain keywords and phrases in a requirements document are tagged and allocated to specific environmental categories. This is aided by the application of ontologies such as SWEET (Semantic Web for Earth and Environmental Terminology from http://sweet.jpl.nasa.gov).').
rdf_(search, ent:description, 'Search the knowledgebase for models.').
rdf_(search, ent:narrative, 'Finding a model needed for a particular purpose is facilitated by various forms of search. A free-form search into the knowldegebase is provided by the search bar in the upper-right corner of the user-interface. This links to knowledge contained within the triple-store knowledgebase, largely independent of semantic context. Other more directed, semantically-driven searches are available from the search page. Links between specific categories of knowledge and models available within the server are contained here. To accomodate this, specific models are tagged and allocated to specific environmental categories. This is aided by the application of ontologies such as SWEET (Semantic Web for Earth and Environmental Terminology from http://sweet.jpl.nasa.gov).').
rdf_(browse, ent:description, 'Browse the knowledebase by category').
rdf_(browse, ent:narrative, 'Environmental and context knowledge follows a natural hierarchical organization. At the top level, we can break out the models into broad categories for Land, Atmosphere, and Aquatic. Below that  level, the specific models are allocated to more finely refined categoies such as terrain roughness. The basic hierearchy follows that of the SWEET ontology (Semantic Web for Earth and Environmental Terminology from http://sweet.jpl.nasa.gov).  ').
rdf_(workflow, ent:description, 'Workflows available to guide the model selection process').
rdf_(workflow, ent:narrative, 'A workflow is defined as a software-guided navigation to problem solving. A composable workflow allows for a sequence of problem solving steps depending on the knowledge available. Several workflows to access probability density function (PDF) environmental models and power spectral density (PSD) models are provided. More information on the concept of semantic and knowledge-based workflows is available from http://www.darpa.mil/WorkArea/DownloadAsset.aspx?id=2147485441.').
rdf_(map, ent:description, 'Navigate the atlas of localities which reference models.').
rdf_(map, ent:narrative, 'The scope of environmental models is world-wide. By their nature, environmental models depend on the particular characteristics of specific geospatial locations.  Where models have locality links we can search regions of interest to find what is available.').
rdf_(ref, ent:description, 'Search for references, citations, and specifications.').
rdf_(ref, ent:narrative, 'Each model has supporting documentation in the form of references and citations. We also distinguish between specifications, foundational research, requirements, and supprting material.  The Zotero citation management system is used to keep track of references and links and we use the SWEET ontology to tag the references with semantic environmental catgories. For example, references relating to aquatic wave energy will get tagged with the phenWave:GravityWave class resource defined in SWEET.').
rdf_(resources, ent:description, 'Supplemental data resources for context models.').
rdf_(resources, ent:narrative, 'A supplemental knowledge-based system will provide semantic web discovery capability. The OSCAR (Ontological System for Context Artifacts and Resources) portal will guide discovery for users to find context models and associated metadata to enable their simulation.  The context models can include collections of PDFs and PSDs.  Context modeling resources include interactive links to tables and supporting doucments, such as environmental regulations, standards, specifications, and typical operational profiles.').
rdf_(features, ent:description, 'This home page contains environmental features along the top icon bar, and process steps as links.').
rdf_(features, ent:narrative, '/html/static_pages/gems/index.html').


rdf_(context, child, land).
rdf_(context, child, aquatic).
rdf_(context, child, atmospheric).
rdf_(context, child, 'physical constants').
rdf_(context, child, 'requirements').
rdf_(land, child, terrain).
rdf_(terrain, child, 'gross terrain').
rdf_('gross terrain', child, slopes).
rdf_('gross terrain', child, elevations).
% rdf_('gross terrain', child, autocorr).
rdf_(terrain, child, 'fine terrain').
rdf_('fine terrain', child, roughness).
rdf_('fine terrain', child, profile).
rdf_(land, child, obstacles).
% rdf_(obstacles, child, 'man-made obstacles').
% rdf_(obstacles, child, 'natural obstacles').
rdf_(aquatic, child, 'sea state').
rdf_('sea state', child, 'wave height').
rdf_('sea state', child, 'wave frequency').
rdf_(aquatic, child, 'lake size').
rdf_(aquatic, child, 'water density').
rdf_(atmospheric, child, 'wind speed').
rdf_(atmospheric, child, rainfall).
rdf_(rainfall, child, cloudiness).
rdf_(rainfall, child, 'rainfall amount').
rdf_(atmospheric, child, temperature).
rdf_(temperature, child, 'daily and seasonal').
rdf_(temperature, child, thermal).
rdf_(atmospheric, child, humidity).
rdf_(atmospheric, child, particulates).
rdf_(particulates, child, 'particle size').
rdf_(particulates, child, 'particle density').
rdf_(atmospheric, child, 'EMI').
rdf_('EMI', child, 'external EM').
rdf_('EMI', child, 'induced EM').
rdf_('EMI', child, 'EMI clutter').
rdf_('external EM', child, 'RF EM').
rdf_('external EM', child, 'EM pulse').
rdf_('external EM', child, 'lightning effects').
rdf_('external EM', child, 'high power microwave').
rdf_('external EM', child, 'electrostatic discharge').
rdf_('external EM', child, 'internally generated EM energy').

rdf_('induced EM', child, 'conducted susceptibility, RF').
rdf_('induced EM', child, 'conducted susceptibility, bulk cable').
rdf_('induced EM', child, 'conducted susceptibility, transients').
rdf_('induced EM', child, 'radiated susceptibility, EF').
rdf_('induced EM', child, 'radiated susceptibility, EMP').

rdf_(context, comment, 'This hierachy describes the categorization of models considered for the context library.').

rdf_(context, link, '/context_browse/browse').

% Linked data
%
rdf_(ex, comment, '').
rdf_(ex, link, '').

rdf_(land, comment, 'Land-based models').
rdf_(aquatic, comment, 'Ocean and inland-water models').
rdf_(atmospheric, comment, 'Atmospheric-based models').

rdf_('terrain', comment, 'Terrain models can either describe surface roughness on a fine scale or topographic elevation changes on a gross scale.').

rdf_('gross terrain', comment, 'Topography and slope models.').
rdf_('gross terrain', link, '/context_select/navigate?category=gross_terrain').

rdf_('fine terrain', comment, 'Terrain models of specific courses and tracks. ').
rdf_('fine terrain', link, '/context_select/navigate?category=fine_terrain').

rdf_(obstacles, comment, 'Structured regular, and irregular obstacles and courses.').
rdf_(obstacles, link, '/context_obstacles/navigate').

rdf_('sea state', comment, 'Models of sea state and aquatic wave height and frequency.').
rdf_('sea state', link, '/context_select/navigate?category=wave').

rdf_('water density', comment, 'Properties of fresh and salt water.').
rdf_('water density', target, target_iframe).
rdf_('water density', link, '/context_water/chart').

rdf_('rainfall', comment, 'Precipation and cloud models').
rdf_('rainfall', link, '/context_select/navigate?category=rainfall').

rdf_('temperature', comment, 'Thermal conductivity and temperature models').
rdf_('temperature', link, '/context_select/navigate?category=thermal').

rdf_('EMI', comment, 'Clutter (noise) and lightning models').
rdf_('EMI', link, '/context_select/navigate?category=clutter').

% Leaf nodes
rdf_(slopes, link, '/context_model/navigate?characteristics=slopes&render=render').
rdf_(slopes, target, target_iframe).
rdf_(slopes, comment, 'PDF models of slope distribution.').

rdf_(elevations, link, '/context_autocorr/navigate').
rdf_(elevations, comment, 'Model of terrain elevation difference.').

rdf_(profile, link, '/context_profile/navigate').
rdf_(profile, comment, 'Models of course and track profiles.').

rdf_('man-made obstacles', link, '/context_obstacles/navigate').
rdf_(profile, comment, 'Models of man-made obstacle profiles.').

rdf_('roughness', link, '/context_psd_workflow/navigate').
rdf_('roughness', comment, 'Models of courses from power spectral density (PSD) datasets.').

rdf_('rainfall rate', link, '/context_model/navigate?characteristics=rainfall&render=render').
rdf_('rainfall rate', target, target_iframe).
rdf_('rainfall rate', comment, 'Models of rainfall intensity').

rdf_('lake size', link, '/context_model/navigate?characteristics=lakeSize&render=render').
rdf_('lake size', target, target_iframe).
rdf_('lake size', comment, 'Models of lake size distribution').

rdf_('wind speed', link, '/context_model/navigate?characteristics=windSpeed&render=render').
rdf_('wind speed', target, target_iframe).
rdf_('wind speed', comment, 'Models of wind speed').

rdf_('wave height', link, '/context_model/navigate?characteristics=waveHeight&render=render').
rdf_('wave height', target, target_iframe).
rdf_('wave height', comment, 'Models of sea-state wave height distribution').

% wave frequency missing in triple store
rdf_('wave frequency', link, '/context_model/navigate?characteristics=waveFrequency&render=render').
rdf_('wave frequency', target, target_iframe).
rdf_('wave frequency', comment, 'Models of wave frequency distribution').

rdf_('particle size', link, '/context_model/navigate?characteristics=particleSizes&render=render').
rdf_('particle size', target, target_iframe).
rdf_('particle size', comment, 'Models of particle size distribution').

rdf_('EMI clutter', link, '/context_model/navigate?characteristics=clutterPower&render=render').
rdf_('EMI clutter', target, target_iframe).
rdf_('EMI clutter', comment, 'Models of clutter power distribution').

rdf_('cloudiness', link, '/context_model/navigate?characteristics=cloudArea&render=render').
rdf_('cloudiness', target, target_iframe).
rdf_('cloudiness', comment, 'Models of cloud coverage').

rdf_('physical constants', link, '/context_resources/navigate').
rdf_('physical constants', comment, 'Properties of static environmental characteristics.').

rdf_('requirements', link, '/context_require/navigate').
rdf_('requirements', comment, 'Navigate to source requirements.').


% Fall-back info
rdf_(_, comment, 'Not finished.').
rdf_(_, link, '').
rdf_(_, target, '').




% not used
/*
rdf_(conus, property, terrain_slopes).
rdf_(conus, characterization, conus_terrain_slope_distro).
rdf_(terrain_slopes, characterized_by, pdf_distribution).
rdf_(terrain_slope_distribution, class,  pdf_distribution).
rdf_(conus_terrain_slope_distro, isa, terrain_slope_distribution).
rdf_(conus_terrain_slope_distro, mean, 1.0).
rdf_(conus_terrain_slope_distro, distribution_type, ent:besselk1).
rdf_(conus_terrain_slope_distro, min_extent, 0.1).
rdf_(conus_terrain_slope_distro, max_extent, 10000.0).
rdf_(conus_terrain_slope_distro, interval_size, 0.1/decade).
rdf_(conus_terrain_slope_distro, x_axis, 'rise/run').
rdf_(conus_terrain_slope_distro, title, 'slope distribution over continental USA').
rdf_(conus_terrain_slope_distro, logarithmic, true).
rdf_(conus_terrain_slope_distro, monte_carlo_model, true).
*/


/*
temperature_records :-
 context:prefix(E),
 context:storeRDF('Wilmington', E, name, 'Wilmington Temperature'),
 context:storeRDF('Wilmington', E, t0, 16.0),
 context:storeRDF('Wilmington', E, ty, 11.5),
 context:storeRDF('Wilmington', E, dT, 1.0),
 context:storeRDF('Wilmington', E, td, 5.0),
 context:storeRDF('Wilmington', E, a, -1.85),
 context:storeRDF('Wilmington', E, b, 4.44),
 context:storeRDF('Wilmington', E, c, -2.4),
 context:storeRDF('Baltimore', E, name, 'Baltimore Temperature'),
 context:storeRDF('Baltimore', E, t0, 13.0),
 context:storeRDF('Baltimore', E, ty, 12.5),
 context:storeRDF('Baltimore', E, dT, 0.8),
 context:storeRDF('Baltimore', E, td, 6.0),
 context:storeRDF('Baltimore', E, a, -1.95),
 context:storeRDF('Baltimore', E, b, 4.44),
 context:storeRDF('Baltimore', E, c, -2.4).

*/



% length units
rdf_(ent:length, ent:units, ent:micron).
rdf_(ent:micron, ent:unit, 'micron').
rdf_(ent:micron, ent:description, 'microns').

rdf_(ent:length, ent:units, ent:nm).
rdf_(ent:nm, ent:unit, 'nm').
rdf_(ent:nm, ent:description, 'nanometers').

rdf_(ent:length, ent:units, ent:mm).
rdf_(ent:mm, ent:unit, 'mm').
rdf_(ent:mm, ent:description, 'millimeters').

rdf_(ent:length, ent:units, ent:cm).
rdf_(ent:cm, ent:unit, 'cm').
rdf_(ent:cm, ent:description, 'centimeters').

rdf_(ent:length, ent:units, ent:inch).
rdf_(ent:inch, ent:unit, 'in').
rdf_(ent:inch, ent:description, 'inches').

rdf_(ent:length, ent:units, ent:mil).
rdf_(ent:mil, ent:unit, 'mil').
rdf_(ent:mil, ent:description, 'mils').

rdf_(ent:length, ent:units, ent:meters).
rdf_(ent:meters, ent:unit, 'm').
rdf_(ent:meters, ent:description, 'meters').

rdf_(ent:length, ent:units, ent:ft).
rdf_(ent:ft, ent:unit, 'ft').
rdf_(ent:ft, ent:description, 'feet').

rdf_(ent:length, ent:units, ent:yards).
rdf_(ent:yards, ent:unit, 'yd').
rdf_(ent:yards, ent:description, 'yards').



% time units
rdf_(ent:time, ent:units, ent:sec).
rdf_(ent:sec, ent:unit, 's').
rdf_(ent:sec, ent:description, 'seconds').

rdf_(ent:time, ent:units, ent:min).
rdf_(ent:min, ent:unit, 'min').
rdf_(ent:min, ent:description, 'minutes').

rdf_(ent:time, ent:units, ent:hour).
rdf_(ent:hour, ent:unit, 'hr').
rdf_(ent:hour, ent:description, 'hours').

rdf_(ent:time, ent:units, ent:day).
rdf_(ent:day, ent:unit, 'day').
rdf_(ent:day, ent:description, 'days').

rdf_(ent:time, ent:units, ent:year).
rdf_(ent:year, ent:unit, 'yr').
rdf_(ent:year, ent:description, 'years').

rdf_(ent:time, ent:units, ent:decade).
rdf_(ent:decade, ent:unit, 'decade').
rdf_(ent:decade, ent:description, 'decades').

rdf_(ent:time, ent:units, ent:mics).
rdf_(ent:mics, ent:unit, 'mics').
rdf_(ent:mics, ent:description, 'micro-seconds').


% Volume units

rdf_(ent:volume, ent:units, ent:m_3).
rdf_(ent:m_3, ent:unit, 'm^3').
rdf_(ent:m_3, ent:description, 'cubic meter').

rdf_(ent:volume, ent:units, ent:cm_3).
rdf_(ent:cm_3, ent:unit, 'cm^3').
rdf_(ent:cm_3, ent:description, 'cubic centimeter').

% Area units

rdf_(ent:area, ent:units, ent:m_2).
rdf_(ent:m_2, ent:unit, 'm^2').
rdf_(ent:m_2, ent:description, 'square meter').

rdf_(ent:area, ent:units, ent:ft_2).
rdf_(ent:ft_2, ent:unit, 'ft^2').
rdf_(ent:ft_2, ent:description, 'square foot').


% Mass units

rdf_(ent:mass, ent:units, ent:kilogram).
rdf_(ent:kilogram, ent:unit, 'kg').
rdf_(ent:kilogram, ent:description, 'kilogram').

rdf_(ent:mass, ent:units, ent:pound).
rdf_(ent:pound, ent:unit, 'lb').
rdf_(ent:pound, ent:description, 'pound(mass)').

% Pressure units
rdf_(ent:pressure, ent:units, ent:atm).
rdf_(ent:atm, ent:unit, 'atm').
rdf_(ent:atm, ent:description, 'atmospheres').

rdf_(ent:pressure, ent:units, ent:torr).
rdf_(ent:torr, ent:unit, 'torr').
rdf_(ent:torr, ent:description, 'Torr').

rdf_(ent:pressure, ent:units, ent:mtorr).
rdf_(ent:mtorr, ent:unit, 'mtorr').
rdf_(ent:atm, ent:description, 'milli Torr').

rdf_(ent:pressure, ent:units, ent:mpa).
rdf_(ent:mpa, ent:unit, 'mpa').
rdf_(ent:mpa, ent:description, 'milli Pascals').

rdf_(ent:pressure, ent:units, ent:pa).
rdf_(ent:pa, ent:unit, 'pa').
rdf_(ent:pa, ent:description, 'Pascals').

rdf_(ent:pressure, ent:units, ent:psi).
rdf_(ent:psi, ent:unit, 'psi').
rdf_(ent:psi, ent:description, 'pounds/in^2').

rdf_(ent:pressure, ent:units, ent:millibar).
rdf_(ent:millibar, ent:unit, 'millibar').
rdf_(ent:millibar, ent:description, 'milli Bar').

rdf_(ent:pressure, ent:units, ent:bar).
rdf_(ent:bar, ent:unit, 'bar').
rdf_(ent:bar, ent:description, 'Bar').

% Temperature units
rdf_(ent:temperature, ent:units, ent:c).
rdf_(ent:c, ent:unit, 'c').
rdf_(ent:c, ent:description, 'Celsius').

rdf_(ent:temperature, ent:units, ent:f).
rdf_(ent:f, ent:unit, 'f').
rdf_(ent:f, ent:description, 'Fahrenheit').

rdf_(ent:temperature, ent:units, ent:k).
rdf_(ent:k, ent:unit, 'k').
rdf_(ent:k, ent:description, 'Kelvin').

rdf_(ent:temperature, ent:units, ent:r).
rdf_(ent:r, ent:unit, 'r').
rdf_(ent:r, ent:description, 'Rankine').


% p_units([mtorr,torr,atm,mpa,pa,psi,millibar,bar]).
% t_units([c,f,k,r]).

% Density units
rdf_(ent:density, ent:units, ent:g_cm3).
rdf_(ent:g_cm3, ent:unit, 'g/cm^3').
rdf_(ent:g_cm3, ent:description, 'grams/cc').

rdf_(ent:density, ent:units, ent:kg_m3).
rdf_(ent:kg_m3, ent:unit, 'kg/m^3').
rdf_(ent:kg_m3, ent:description, 'kilos/cubic meter').

rdf_(ent:density, ent:units, ent:lb_ft3).
rdf_(ent:lb_ft3, ent:unit, 'lb/ft^3').
rdf_(ent:lb_ft3, ent:description, 'pounds/cubic foot').

rdf_(ent:density, ent:units, ent:oz_in3).
rdf_(ent:oz_in3, ent:unit, 'oz/in^3').
rdf_(ent:oz_in3, ent:description, 'ounces/cubic inch').

rdf_(ent:density, ent:units, ent:tons_km3).
rdf_(ent:tons_km3, ent:unit, 'ton/km^3').
rdf_(ent:tons_km3, ent:description, 'tons/cubic km').

rdf_(ent:density, ent:units, ent:mg_micron3).
rdf_(ent:mg_micron3, ent:unit, 'mg/micron^3').
rdf_(ent:mg_micron3, ent:description, 'mils/micron^3').

rdf_(ent:density, ent:units, ent:kg_liter).
rdf_(ent:kg_liter, ent:unit, 'kg/dm^3').
rdf_(ent:kg_liter, ent:description, 'kilos/liter').

% Scientific

rdf_(ent:water, ent:molecular_weight, 18.015268*au).
rdf_(ent:water, ent:deltaHvap, 9717.1*cal).
rdf_(ent:water, ent:deltaSvap, 26.04*cal/k).

rdf_(ent:standardAtmosphere, ent:description, 'Specification of standard atmosphere').

rdf_(ent:standardAtmosphere, ent:temperature, 70*f).
rdf_(ent:standardAtmosphere, ent:temperature, 273.15*k).
rdf_(ent:standardAtmosphere, ent:pressure, 1*atm).
rdf_(ent:standardAtmosphere, ent:dryAdiabaticLapseRate, 9.8*c/km).
rdf_(ent:standardAtmosphere, ent:moistAdiabaticLapseRateTypical, 5.0*c/km).
rdf_(ent:standardAtmosphere, ent:seaLevel, 0.0*km).
rdf_(ent:standardAtmosphere, ent:dryAdiabaticPressureHead, -0.116*km).
% rdf_(ent:standardAtmosphere, ent:troposphericLimit, 0.1*atm).
rdf_(ent:standardAtmosphere, ent:molecularWeight, 28.966*au). %
rdf_(ent:standardAtmosphere, ent:specificHeatRatio, 1.4).
rdf_(ent:standardAtmosphere, ent:specificGasConstantDryAir, 287*j/kg/k).
rdf_(ent:standardAtmosphere, ent:specificGasConstantWaterVapor, 462*j/kg/k).

rdf_(ent:solar, ent:averageSolarInsolation, 1366*w/m^2).
rdf_(ent:solar, ent:averageAlbedo, 0.31).


rdf_(ent:physicalConstants, ent:gasConstant, 1.9858775*cal/k/mol).
rdf_(ent:physicalConstants, ent:gasConstant, 8.314472*j/k/mol).
rdf_(ent:physicalConstants, ent:atomicUnit, 1.66053886e-24*g).
rdf_(ent:physicalConstants, ent:avogadrosNumber, 6.02214129e23*n).
rdf_(ent:physicalConstants, ent:stefanBoltzmann, 5.670373e-8*w/m^2/k^4).
rdf_(ent:physicalConstants, ent:gravity, 9.80665*m/s^2).
rdf_(ent:physicalConstants, ent:gravity, 32.174049*ft/s^2).
rdf_(ent:physicalConstants, ent:boltzmannConstant, 1.38065e-23*j/k).
rdf_(ent:physicalConstants, ent:electricalCharge, 1.60218e-19*coulombs).
rdf_(ent:physicalConstants, ent:planckConstant, 6.62607e-34*j*s).
rdf_(ent:physicalConstants, ent:faradaysConstant, 96485.3383*coulombs/mol).
rdf_(ent:physicalConstants, ent:dielectricConstant, 8.854187e-12*fd/m).
rdf_(ent:physicalConstants, ent:speedLight, 3.0e8*m/s).


rdf_('allegheny', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 8, 101, 363, 556, 555, 560, 609, 632, 693, 761, 937, 978, 984, 775, 941, 864, 784, 662, 602, 416, 289, 168, 141, 59, 36, 10, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('arkansas', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 50, 40, 137, 29, 36, 50, 10, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('chattooga', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 71, 616, 1987, 3304, 3425, 3064, 2555, 2345, 2150, 1971, 1577, 1197, 944, 712, 415, 337, 222, 147, 120, 85, 54, 36, 27, 16, 8, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('hudson', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 1, 11, 9, 32, 58, 180, 392, 768, 1170, 1970, 3086, 4431, 4819, 4367, 3944, 3422, 3027, 2399, 1644, 1158, 721, 335, 109, 29, 7, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('merrimack', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 31, 64, 121, 159, 174, 347, 671, 1283, 1849, 2251, 2473, 2742, 3044, 3109, 3131, 2607, 2319, 2057, 1601, 1086, 754, 413, 154, 38, 11, 8, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('mississippi', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 7, 123, 380, 1162, 2627, 5230, 6138, 6834, 7081, 6424, 6320, 4547, 3598, 2559, 1248, 424, 100, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('mississippi_anoka', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 59, 199, 549, 565, 885, 1316, 2186, 2674, 3810, 3697, 3164, 2801, 2101, 1728, 1496, 1108, 669, 338, 146, 44, 26, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('ohio', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 22, 109, 160, 328, 661, 1263, 1838, 2776, 3023, 2845, 2641, 2775, 2472, 2331, 2152, 2101, 1837, 1003, 296, 17, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('salt', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 40, 343, 799, 1853, 4069, 5350, 3824, 2619, 2268, 1945, 1550, 1335, 1237, 1007, 864, 804, 709, 521, 429, 258, 124, 82, 32, 24, 16, 10, 16, 7, 5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('stcroix', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 52, 286, 636, 1879, 2708, 2447, 1589, 1141, 787, 533, 502, 409, 341, 237, 149, 87, 44, 18, 5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).
rdf_('umpqua', ent:river, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 312, 2409, 4540, 4164, 2242, 1668, 1750, 1997, 2384, 2597, 2765, 2646, 2330, 1896, 1552, 952, 636, 452, 285, 188, 99, 70, 21, 20, 7, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).

