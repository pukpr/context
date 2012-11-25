:- multifile(rdf_/3).
:- multifile(ref_/3).


ref_m('fine terrain features',     model, '/context_select/navigate?category=fine_terrain').
ref_m('fine terrain features',     target_iframe, '/html/static_pages/gems/fine_terrain.png').
ref_m('fine terrain features',     category, 'propMass:Roughness').
ref_m('fine terrain features',     category, 'realmSoil:SoilLayer').
ref_m('fine terrain features',     category, 'realmBiolBiome:Terrain').
ref_m('fine terrain features',     category, 'reprSpaceGeometry:Surface').

ref_m('gross terrain features',     model, '/context_select/navigate?category=gross_terrain').
ref_m('gross terrain features',     target_iframe, '/html/static_pages/gems/gross_terrain.png').
ref_m('gross terrain features',     category, 'propSpaceHeight:Topography').
ref_m('gross terrain features',     category, 'realmBiolBiome:Terrain').
ref_m('gross terrain features',     category, 'reprSpaceGeometry:Surface').

ref_m('wave features',     model, '/context_select/navigate?category=wave').
ref_m('wave features',     target_iframe, '/html/static_pages/gems/wave_statistics.png').
ref_m('wave features',     category, 'phenWave:GravityWave').
ref_m('wave features',     category, 'propSpaceHeight:SeaState').
ref_m('wave features',     category, 'phenOcean:OceanPhenomena').
ref_m('wave features',     category, 'phenFluidDynamics:FluidPhenomena').

ref_m('wind features',     model, '/context_select/navigate?category=wind').
ref_m('wind features',     target_iframe, '/html/static_pages/gems/wind_statistics.png').
ref_m('wind features',     category, 'phenAtmoWind:Wind').
ref_m('wind features',     category, 'phenAtmo:Weather').
ref_m('wind features',     category, 'phenFluidDynamics:FluidPhenomena').

ref_m('clutter features',     model, '/context_select/navigate?category=clutter').
ref_m('clutter features',     target_iframe, '/html/static_pages/gems/clutter_modeling.png').
ref_m('clutter features',     category, 'phenAtmoLightning:Lightning').
ref_m('clutter features',     category, 'phenElecMag:ElectricField').
ref_m('clutter features',     category, 'phenWaveNoise:Noise').
ref_m('clutter features',     category, 'matrEquipment:Communications').

ref_m('lakes features',     model, '/context_select/navigate?category=lakes').
ref_m('lakes features',     target_iframe, '/html/static_pages/gems/lake_sizes.png').
ref_m('lakes features',     category, 'phenHydro:WaterFlow').
ref_m('lakes features',     category, 'realmHydroBody:Lake').

ref_m('particle features',     model, '/context_select/navigate?category=particles').
ref_m('particle features',     target_iframe, '/html/static_pages/gems/particle_sizes.png').
ref_m('particle features',     category, 'matrAerosol:Particulate').
ref_m('particle features',     category, 'phenAtmoPrecipitation:Rainfall').
ref_m('particle features',     category, 'phenGeolVolcano:VolcanicPlume').
ref_m('particle features',     category, 'statePhysical:Droplet').

ref_m('thermal features',     model, '/context_select/navigate?category=thermal').
ref_m('thermal features',     target_iframe, '/html/static_pages/gems/thermal_dispersion.png').
ref_m('thermal features',     category, 'phenMixing:Diffusion').
ref_m('thermal features',     category, 'propConductivity:ThermalConductivity').
ref_m('thermal features',     category, 'propDiffusivity:ThermalDiffusivity').
ref_m('thermal features',     category, 'propTemperature:Temperature').
ref_m('thermal features',     category, 'phenAtmo:Thermal').

ref_m('rain features',     model, '/context_select/navigate?category=rainfall').
ref_m('rain features',     target_iframe, '/html/static_pages/gems/rain_statistics.png').
ref_m('rain features',     category, 'phenAtmoCloud:Cloud').
ref_m('rain features',     category, 'phenAtmo:Weather').
ref_m('rain features',     category, 'phenAtmoPrecipitation:Rainfall').

ref_m('corrosion features',     model, '/context_select/navigate?category=corrosion').
ref_m('corrosion features',     target_iframe, '/html/static_pages/gems/corrosion_oxidation.png').
ref_m('corrosion features',     category, 'procChemical:Corrosion').
ref_m('corrosion features',     category, 'procChemical:Oxidation').
ref_m('corrosion features',     category, 'matrElement:Fe').
ref_m('corrosion features',     category, 'phenSolid:Ablation').

ref_m('Dynamic Context Server',     model, '/app').
ref_m('Dynamic Context Server',     target_iframe, '/html/static_pages/gems/').
ref_m('Dynamic Context Server',     category, 'realm:Land').
ref_m('Dynamic Context Server',     category, 'realm:Atmosphere').
ref_m('Dynamic Context Server',     category, 'realm:Ocean').
ref_m('Dynamic Context Server',     category, 'phenSystemComplexity:Pattern').
ref_m('Dynamic Context Server',     category, 'reprMathStatistics:Statistics').
ref_m('Dynamic Context Server',     category, 'stateThermodynamic:MaximumEntropy').

ref_m('requirements',     model, '/context_require/navigate').
ref_m('requirements',     target_iframe, '/html/requirements.html').
ref_m('requirements',     category, 'humanDecision:Allocation').
ref_m('requirements',     category, 'humanDecision:Objective').

ref_m('references',     model, '/context_ref/navigate').
ref_m('references',     target_iframe, '/html/citations.html').
ref_m('references',     category, 'humanResearch:Publication').
ref_m('references',     category, 'humanEnvirStandards:EnvironmentalStandard').
ref_m('references',     category, 'humanDecision:Allocation').
ref_m('references',     category, 'humanDecision:Objective').
ref_m('references',     category, 'repr:Knowledge').

ref_m('map',     model, '/context_map/search').
ref_m('map',     target_iframe, '/html/map.html').
ref_m('map',     category, 'realmLandform:LandRegion').
ref_m('map',     category, 'reprDataServiceGeospatial:WebFeatureServer').

ref_m('resources',     model, '/context_resources/navigate').
ref_m('resources',     target_iframe, '/html/resources.html').
ref_m('resources',     category, 'phenAtmo:Sunlight').
ref_m('resources',     category, 'humanEnvirStandards:EnvironmentalStandard').
ref_m('resources',     category, 'phenAtmo:Weather').
ref_m('resources',     category, 'repr:sciUnits').
ref_m('resources',     category, 'reprDataService:DataService').

ref_m('workflow',     model, '/context_workflow/workflows').
ref_m('workflow',     target_iframe, '/html/workflow.html').
ref_m('workflow',     category, 'reprDataService:DataService').
ref_m('workflow',     category, 'phenSystemComplexity:Pattern').
ref_m('workflow',     category, 'humanDecision:Objective').
ref_m('workflow',     category, 'humanDecision:Allocation').
ref_m('workflow',     category, 'humanEnvirStandards:EnvironmentalStandard').

ref_m('search',     model, '/context_search/search').
ref_m('search',     target_iframe, '/html/search.html').
ref_m('search',     category, 'reprDataService:DataService').
ref_m('search',     category, 'reprMathFunction:ProbabilityDensityFunction').
ref_m('search',     category, 'phenSystemComplexity:Pattern').
ref_m('search',     category, 'humanDecision:Objective').
ref_m('search',     category, 'humanDecision:Allocation').
ref_m('search',     category, 'humanEnvirStandards:EnvironmentalStandard').

ref_m('AR7038',     model, '/context_climate_AR7038/navigate').
ref_m('AR7038',     target_iframe, '/html/climate_AR7038.html').
ref_m('AR7038',     category, 'phenAtmo:Weather').
ref_m('AR7038',     category, 'realm:Atmosphere').
ref_m('AR7038',     category, 'realm:Cryosphere').
ref_m('AR7038',     category, 'humanEnvirStandards:EnvironmentalStandard').
ref_m('AR7038',     category, 'humanDecision:Objective').

ref_m('profiles',     model, '/context_profile/navigate').
ref_m('profiles',     target_iframe, '/html/profile.html').
ref_m('profiles',     category, 'reprSpaceGeometry:Surface').
ref_m('profiles',     category, 'propMass:Roughness').
ref_m('profiles',     category, 'realmBiolBiome:Terrain').
ref_m('profiles',     category, 'reprDataServiceAnalysis:FourierTransform').
ref_m('profiles',     category, 'reprDataServiceAnalysis:PowerSpectrum').
ref_m('profiles',     category, 'reprDataServiceAnalysis:SpectralAnalysis').

ref_m('PSD',     model, '/context_psd_workflow/navigate').
ref_m('PSD',     target_iframe, '/html/psd_workflow.html').
ref_m('PSD',     category, 'realmSoil:SoilLayer').
ref_m('PSD',     category, 'propMass:Roughness').
ref_m('PSD',     category, 'reprDataServiceAnalysis:FourierTransform').
ref_m('PSD',     category, 'reprDataServiceAnalysis:PowerSpectrum').
ref_m('PSD',     category, 'reprDataServiceAnalysis:SpectralAnalysis').

ref_m('obstacles',     model, '/context_obstacles/navigate').
ref_m('obstacles',     target_iframe, '/html/obstacles.html').
ref_m('obstacles',     category, 'reprSpaceGeometry:Surface').

ref_m('autocorrelation',     model, '/context_autocorr/navigate').
ref_m('autocorrelation',     target_iframe, '/html/autocorr.html').
ref_m('autocorrelation',     category, 'propSpaceHeight:Topography').
ref_m('autocorrelation',     category, 'propFraction:Correlation').
ref_m('autocorrelation',     category, 'reprMathFunction:ProbabilityDensityFunction').
ref_m('autocorrelation',     category, 'propFraction:Probability').

ref_m('water',     model, '/context_water/navigate').
ref_m('water',     target_iframe, '/html/water.html').
ref_m('water',     category, 'phenOcean:OceanPhenomena').
ref_m('water',     category, 'realm:Hydrosphere').
ref_m('water',     category, 'statePhysical:Droplet').
ref_m('water',     category, 'matrWater:WaterSubstance').

ref_m('seastate',     model, '/context_seastate/navigate').
ref_m('seastate',     target_iframe, '/html/seastate.html').
ref_m('seastate',     category, 'phenWave:GravityWave').
ref_m('seastate',     category, 'propSpaceHeight:SeaState').
ref_m('seastate',     category, 'phenOcean:OceanPhenomena').

ref_m('lightning',     model, '/context_lightning/navigate').
ref_m('lightning',     target_iframe, '/html/lightning.html').
ref_m('lightning',     category, 'phenAtmoLightning:Lightning').

ref_m('electro-magnetic',     model, '/context_emi/navigate').
ref_m('electro-magnetic',     target_iframe, '/html/emi.html').
ref_m('electro-magnetic',     category, 'phenElecMag:ElectricField').
ref_m('electro-magnetic',     category, 'matrEquipment:Communications').

ref_m('clutter',     model, '/context_clutter/navigate').
ref_m('clutter',     target_iframe, '/html/clutter.html').
ref_m('clutter',     category, 'phenWaveNoise:Noise').
ref_m('clutter',     category, 'matrEquipment:Communications').

ref_m('fording',     model, '/context_fording/navigate').
ref_m('fording',     target_iframe, '/html/fording.html').
ref_m('fording',     category, 'phenHydro:WaterFlow').

ref_m('lakes',     model, '/context_lakes/navigate').
ref_m('lakes',     target_iframe, '/html/lakes.html').
ref_m('lakes',     category, 'realmHydroBody:Lake').

ref_m('thermal',     model, '/context_thermal/navigate').
ref_m('thermal',     target_iframe, '/html/thermal.html').
ref_m('thermal',     category, 'phenMixing:Diffusion').
ref_m('thermal',     category, 'propConductivity:ThermalConductivity').
ref_m('thermal',     category, 'propDiffusivity:ThermalDiffusivity').
ref_m('thermal',     category, 'propTemperature:Temperature').
ref_m('thermal',     category, 'phenAtmo:Thermal').

ref_m('corrosion',     model, '/context_corrosion/navigate').
ref_m('corrosion',     target_iframe, '/html/corrosion.html').
ref_m('corrosion',     category, 'procChemical:Corrosion').
ref_m('corrosion',     category, 'procChemical:Oxidation').
ref_m('corrosion',     category, 'matrElement:Fe').
ref_m('corrosion',     category, 'phenSolid:Ablation').

ref_m('demos',     model, '/context_demos/navigate').
ref_m('demos',     target_iframe, '/html/demos.html').

ref_m('atmosphere',     model, '/context_atm/navigate').
ref_m('atmosphere',     target_iframe, '/html/atm.html').
ref_m('atmosphere',     category, 'phenAtmo:Weather').
ref_m('atmosphere',     category, 'phenAtmoPressure:Barometric').
ref_m('atmosphere',     category, 'propFraction:Humidity').
ref_m('atmosphere',     category, 'propPressure:BarometricPressure').
ref_m('atmosphere',     category, 'phenAtmo:Thermal').
ref_m('atmosphere',     category, 'propTemperature:Temperature').

ref_m('solar',     model, '/context_solar/navigate').
ref_m('solar',     target_iframe, '/html/solar.html').
ref_m('solar',     category, 'phenAtmo:Sunlight').
ref_m('solar',     category, 'phenElecMag:ElectricField').

ref_m('browse',     model, '/context_browse/browse').
ref_m('browse',     target_iframe, '/html/browse.html').
ref_m('browse',     category, 'realm:Land').
ref_m('browse',     category, 'realm:Atmosphere').
ref_m('browse',     category, 'realm:Ocean').
ref_m('browse',     category, 'humanEnvirStandards:EnvironmentalStandard').
ref_m('browse',     category, 'reprDataService:DataService').
ref_m('browse',     category, 'phen:StochasticProcess').


ref_(URL, target_iframe, Page) :-
    ref_m(ID, model, URL),
    ref_m(ID, target_iframe, Page), !.

ref_(URL, target_iframe, Page) :-
    atom_concat('/context_browse/browse?term=', Term, URL),
    atom_concat('/html/browse.html#', Term, Page), !.

ref_(_, IName, Page) :-
    atomic_list_concat(['/html/',IName,'.html'], Page).



% ----------------------------------------------------



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
rdf_(example, ent:description, 'Examples of use.').
rdf_(example, ent:narrative, 'Examples of model use').


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
% rdf_('EMI', child, 'external EM').
% rdf_('EMI', child, 'induced EM').
rdf_('EMI', child, 'EMI clutter').
rdf_('EMI', child, 'RF EM').
rdf_('EMI', child, 'EM pulse').
rdf_('EMI', child, 'lightning effects').
rdf_('EMI', child, 'microwave').
rdf_('EMI', child, 'ESD').
rdf_('EMI', child, 'internal EM energy').
/*
rdf_('induced EM', child, 'conducted susceptibility, RF').
rdf_('induced EM', child, 'conducted susceptibility, bulk cable').
rdf_('induced EM', child, 'conducted susceptibility, transients').
rdf_('induced EM', child, 'radiated susceptibility, EF').
rdf_('induced EM', child, 'radiated susceptibility, EMP').
*/

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

% ------------- SemiMarkov

rdf_(corrugations, alpha1, 1.0).
rdf_(corrugations, l1, 5.0).
rdf_(corrugations, alpha2, 1.0).
rdf_(corrugations, l2, 27.5).
rdf_(corrugations, weight, 1.0).

/*
rdf_(gero_corrugations, alpha1, 0.01).
rdf_(gero_corrugations, l1, 0.11).
rdf_(gero_corrugations, alpha2, 0.01).
rdf_(gero_corrugations, l2, 0.605).
rdf_(gero_corrugations, weight, 0.01).
*/

rdf_(gero_corrugations, alpha1, 0.02).
rdf_(gero_corrugations, l1, 0.11).
rdf_(gero_corrugations, alpha2, 0.02).
rdf_(gero_corrugations, l2, 0.61).
rdf_(gero_corrugations, weight, 0.01).

rdf_(gero_corrugations_r, alpha1, 40.0).
rdf_(gero_corrugations_r, l1, 0.0).
rdf_(gero_corrugations_r, alpha2, 40.0).
rdf_(gero_corrugations_r, l2, 0.0).
rdf_(gero_corrugations_r, weight, 0.005).

rdf_(gero_corrugations_s, alpha1, 0.3).
rdf_(gero_corrugations_s, l1, 0.3).
rdf_(gero_corrugations_s, alpha2, 0.3).
rdf_(gero_corrugations_s, l2, 0.315).
rdf_(gero_corrugations_s, weight, 0.002).

rdf_(gero_corrugations_x, alpha1, 0.009).
rdf_(gero_corrugations_x, l1, 0.009).
rdf_(gero_corrugations_x, alpha2, 0.009).
rdf_(gero_corrugations_x, l2, 0.009).
rdf_(gero_corrugations_x, weight, 0.003).


rdf_(gero_belgian, alpha1, 0.0133).
rdf_(gero_belgian, l1, 0.13).
rdf_(gero_belgian, alpha2, 0.0133).
rdf_(gero_belgian, l2, 0.02).
rdf_(gero_belgian, weight, 0.006).

rdf_(gero_belgian_1, alpha1, 0.08).
rdf_(gero_belgian_1, l1, 0.133).
rdf_(gero_belgian_1, alpha2, 0.08).
rdf_(gero_belgian_1, l2, 0.133).
rdf_(gero_belgian_1, weight, 0.003).

rdf_(gero_belgian_2, alpha1, 0.54).
rdf_(gero_belgian_2, l1, 0.133).
rdf_(gero_belgian_2, alpha2, 0.54).
rdf_(gero_belgian_2, l2, 0.133).
rdf_(gero_belgian_2, weight, 0.005).

rdf_(gero_belgian_3, alpha1, 5.4).
rdf_(gero_belgian_3, l1, 0.133).
rdf_(gero_belgian_3, alpha2, 5.4).
rdf_(gero_belgian_3, l2, 0.133).
rdf_(gero_belgian_3, weight, 0.005).


rdf_(gero_fatigue, alpha1, 0.75).
rdf_(gero_fatigue, l1, 0.5).
rdf_(gero_fatigue, alpha2, 0.75).
rdf_(gero_fatigue, l2, 0.5).
rdf_(gero_fatigue, weight, 0.015). %

rdf_(gero_fatigue_s, alpha1, 1.4).
rdf_(gero_fatigue_s, l1, 2.0).  %2x
rdf_(gero_fatigue_s, alpha2, 0.04).
rdf_(gero_fatigue_s, l2, 0.2).
rdf_(gero_fatigue_s, weight, 0.012).

rdf_(gero_fatigue_p, alpha1, 0.04).
rdf_(gero_fatigue_p, l1, 0.2).
rdf_(gero_fatigue_p, alpha2, 1.4).
rdf_(gero_fatigue_p, l2, 2.0).
rdf_(gero_fatigue_p, weight, 0.012).

rdf_(gero_fatigue_x, alpha1, 0.006).
rdf_(gero_fatigue_x, l1, 0.008).
rdf_(gero_fatigue_x, alpha2, 0.006).
rdf_(gero_fatigue_x, l2, 0.008).
rdf_(gero_fatigue_x, weight, 0.01). %

rdf_(gero_fatigue_r, alpha1, 10.0).
rdf_(gero_fatigue_r, l1, 0.0).
rdf_(gero_fatigue_r, alpha2, 10.0).
rdf_(gero_fatigue_r, l2, 0.0).
rdf_(gero_fatigue_r, weight, 0.015).

/*
rdf_(gero_pothole, alpha1, 0.1). %1.5
rdf_(gero_pothole, l1, 0.9). %7.5
rdf_(gero_pothole, alpha2, 0.1). % 0.02
rdf_(gero_pothole, l2, 0.9).
rdf_(gero_pothole, weight, 0.06).
*/
rdf_(gero_pothole, alpha1, 1.5). %1.5
rdf_(gero_pothole, l1, 7.5). %7.5
rdf_(gero_pothole, alpha2, 0.03). % 0.02
rdf_(gero_pothole, l2, 0.57).  % 0.55
rdf_(gero_pothole, weight, 0.05).

rdf_(gero_pothole_r, alpha1, 40.0).
rdf_(gero_pothole_r, l1, 0.0).
rdf_(gero_pothole_r, alpha2, 40.0).
rdf_(gero_pothole_r, l2, 0.0).
rdf_(gero_pothole_r, weight, 0.01).

rdf_(gero_pothole_s, alpha1, 5.0).
rdf_(gero_pothole_s, l1, 1.0).
rdf_(gero_pothole_s, alpha2, 5.0).
rdf_(gero_pothole_s, l2, 1.0).
rdf_(gero_pothole_s, weight, 0.005).

rdf_(gero_pothole_x, alpha1, 0.5).
rdf_(gero_pothole_x, l1, 0.1).
rdf_(gero_pothole_x, alpha2, 0.5).
rdf_(gero_pothole_x, l2, 0.1).
rdf_(gero_pothole_x, weight, 0.002).


rdf_(mercedes_benz, alpha1, 0.0133).
rdf_(mercedes_benz, l1, 0.13).
rdf_(mercedes_benz, alpha2, 0.0133).
rdf_(mercedes_benz, l2, 0.02).
rdf_(mercedes_benz, weight, 0.002).

rdf_(mercedes_benz_2, alpha1, 0.54).
rdf_(mercedes_benz_2, l1, 0.133).
rdf_(mercedes_benz_2, alpha2, 0.54).
rdf_(mercedes_benz_2, l2, 0.133).
rdf_(mercedes_benz_2, weight, 0.002).

rdf_(mercedes_benz_3, alpha1, 20.0).
rdf_(mercedes_benz_3, l1, 0.133).
rdf_(mercedes_benz_3, alpha2, 20.0).
rdf_(mercedes_benz_3, l2, 0.133).
rdf_(mercedes_benz_3, weight, 0.015).




rdf_(mn_big_ravines, alpha1, 2000.0).
rdf_(mn_big_ravines, l1, 800.0).
rdf_(mn_big_ravines, alpha2, 80.0).
rdf_(mn_big_ravines, l2, 80.0).
rdf_(mn_big_ravines, weight, 8.0).

rdf_(mn_small_ravines, alpha1, 500.0).
rdf_(mn_small_ravines, l1, 200.0).
rdf_(mn_small_ravines, alpha2, 20.0).
rdf_(mn_small_ravines, l2, 20.0).
rdf_(mn_small_ravines, weight, 8.0).

rdf_(mn_roughness, alpha1, 30.0).
rdf_(mn_roughness, l1, 2.0).
rdf_(mn_roughness, alpha2, 30.0).
rdf_(mn_roughness, l2, 10.0).
rdf_(mn_roughness, weight, 0.5).


rdf_(ou_roughness, alpha1, 71.5).
rdf_(ou_roughness, l1, 0.0).
rdf_(ou_roughness, alpha2, 71.5).
rdf_(ou_roughness, l2, 0.0).
rdf_(ou_roughness, weight, 50.0).

rdf_(sm_roughness, alpha1, 71.5).
rdf_(sm_roughness, l1, 0.0).
rdf_(sm_roughness, alpha2, 71.5).
rdf_(sm_roughness, l2, 0.0).
rdf_(sm_roughness, weight, 1.0).


rdf_(rock_roughness_semi, alpha1, 0.5).
rdf_(rock_roughness_semi, l1, 0.0).
rdf_(rock_roughness_semi, alpha2, 0.5).
rdf_(rock_roughness_semi, l2, 0.0).
rdf_(rock_roughness_semi, weight, 0.1).

rdf_(rock_roughness_ou, diffusion, 0.0002).
rdf_(rock_roughness_ou, drag, 0.01).
rdf_(rock_roughness_ou, spacing, 1.0).
rdf_(rock_roughness_ou, weight, 1.0).

