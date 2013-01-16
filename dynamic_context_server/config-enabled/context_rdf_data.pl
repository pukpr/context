/** <module> RDF-emulated data for context features
    * Category features
    * Information and help snippets
*/


:- multifile(rdf_/3).
:- multifile(ref_/3).


%%   ref_(+URL, +Type, -Feature)
%
%    Internal triple mechanism to map feature
%%   ref_m(+N, +Type, -Path)
%
%    Internal triple mechanism to map feature path
ref_m('fine terrain features',     model, '/context_select/navigate?category=fine_terrain').
ref_m('fine terrain features',     target_iframe, '/html/static_pages/gems/fine_terrain.html').
ref_m('fine terrain features',     category, 'propMass:Roughness').
ref_m('fine terrain features',     category, 'realmSoil:SoilLayer').
ref_m('fine terrain features',     category, 'realmBiolBiome:Terrain').
ref_m('fine terrain features',     category, 'reprSpaceGeometry:Surface').

ref_m('gross terrain features',     model, '/context_select/navigate?category=gross_terrain').
ref_m('gross terrain features',     target_iframe, '/html/static_pages/gems/gross_terrain.html').
ref_m('gross terrain features',     category, 'propSpaceHeight:Topography').
ref_m('gross terrain features',     category, 'realmBiolBiome:Terrain').
ref_m('gross terrain features',     category, 'reprSpaceGeometry:Surface').

ref_m('wave features',     model, '/context_select/navigate?category=wave').
ref_m('wave features',     target_iframe, '/html/static_pages/gems/wave_statistics.html').
ref_m('wave features',     category, 'phenWave:GravityWave').
ref_m('wave features',     category, 'propSpaceHeight:SeaState').
ref_m('wave features',     category, 'phenOcean:OceanPhenomena').
ref_m('wave features',     category, 'phenFluidDynamics:FluidPhenomena').

ref_m('wind features',     model, '/context_select/navigate?category=wind').
ref_m('wind features',     target_iframe, '/html/static_pages/gems/wind_statistics.html').
ref_m('wind features',     category, 'phenAtmoWind:Wind').
ref_m('wind features',     category, 'phenAtmo:Weather').
ref_m('wind features',     category, 'phenFluidDynamics:FluidPhenomena').

ref_m('clutter features',     model, '/context_select/navigate?category=clutter').
ref_m('clutter features',     target_iframe, '/html/static_pages/gems/clutter_modeling.html').
ref_m('clutter features',     category, 'phenAtmoLightning:Lightning').
ref_m('clutter features',     category, 'phenElecMag:ElectricField').
ref_m('clutter features',     category, 'phenWaveNoise:Noise').
ref_m('clutter features',     category, 'matrEquipment:Communications').

ref_m('lakes features',     model, '/context_select/navigate?category=lakes').
ref_m('lakes features',     target_iframe, '/html/static_pages/gems/lake_sizes.html').
ref_m('lakes features',     category, 'phenHydro:WaterFlow').
ref_m('lakes features',     category, 'realmHydroBody:Lake').

ref_m('particle features',     model, '/context_select/navigate?category=particles').
ref_m('particle features',     target_iframe, '/html/static_pages/gems/particle_sizes.html').
ref_m('particle features',     category, 'matrAerosol:Particulate').
ref_m('particle features',     category, 'phenAtmoPrecipitation:Rainfall').
ref_m('particle features',     category, 'phenGeolVolcano:VolcanicPlume').
ref_m('particle features',     category, 'statePhysical:Droplet').

ref_m('thermal features',     model, '/context_select/navigate?category=thermal').
ref_m('thermal features',     target_iframe, '/html/static_pages/gems/thermal_dispersion.html').
ref_m('thermal features',     category, 'phenMixing:Diffusion').
ref_m('thermal features',     category, 'propConductivity:ThermalConductivity').
ref_m('thermal features',     category, 'propDiffusivity:ThermalDiffusivity').
ref_m('thermal features',     category, 'propTemperature:Temperature').
ref_m('thermal features',     category, 'phenAtmo:Thermal').

ref_m('rain features',     model, '/context_select/navigate?category=rainfall').
ref_m('rain features',     target_iframe, '/html/static_pages/gems/rain_statistics.html').
ref_m('rain features',     category, 'phenAtmoCloud:Cloud').
ref_m('rain features',     category, 'phenAtmo:Weather').
ref_m('rain features',     category, 'phenAtmoPrecipitation:Rainfall').

ref_m('corrosion features',     model, '/context_select/navigate?category=corrosion').
ref_m('corrosion features',     target_iframe, '/html/static_pages/gems/corrosion_oxidation.html').
ref_m('corrosion features',     category, 'procChemical:Corrosion').
ref_m('corrosion features',     category, 'procChemical:Oxidation').
ref_m('corrosion features',     category, 'matrElement:Fe').
ref_m('corrosion features',     category, 'phenSolid:Ablation').

ref_m('Dynamic Context Server',     model, '/app').
ref_m('Dynamic Context Server',     model, '/').
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

ref_m('map',     model, '/context_map/navigate').
ref_m('map',     target_iframe, '/html/map.html').
ref_m('map',     category, 'realmLandform:LandRegion').
ref_m('map',     category, 'reprDataServiceGeospatial:WebFeatureServer').

ref_m('query',     model, '/context_query/navigate').
ref_m('query',     target_iframe, '/html/query.html').
ref_m('query',     category, 'reprDataServiceGeospatial:WebFeatureServer').
ref_m('query',     category, 'reprDataService:DataService').
ref_m('query',     category, 'repr:Knowledge').

ref_m('resources',     model, '/context_resources/navigate').
ref_m('resources',     target_iframe, '/html/resources.html').
ref_m('resources',     category, 'phenAtmo:Sunlight').
ref_m('resources',     category, 'humanEnvirStandards:EnvironmentalStandard').
ref_m('resources',     category, 'phenAtmo:Weather').
ref_m('resources',     category, 'repr:sciUnits').
ref_m('resources',     category, 'reprDataService:DataService').

ref_m('workflow',     model, '/context_workflow/navigate').
ref_m('workflow',     target_iframe, '/html/workflow.html').
ref_m('workflow',     category, 'reprDataService:DataService').
ref_m('workflow',     category, 'phenSystemComplexity:Pattern').
ref_m('workflow',     category, 'humanDecision:Objective').
ref_m('workflow',     category, 'humanDecision:Allocation').
ref_m('workflow',     category, 'humanEnvirStandards:EnvironmentalStandard').

ref_m('search',     model, '/context_search/navigate').
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
ref_m('profiles',     feature, 'fine_terrain').
ref_m('profiles',     category, 'reprSpaceGeometry:Surface').
ref_m('profiles',     category, 'propMass:Roughness').
ref_m('profiles',     category, 'realmBiolBiome:Terrain').
ref_m('profiles',     category, 'reprDataServiceAnalysis:FourierTransform').
ref_m('profiles',     category, 'reprDataServiceAnalysis:PowerSpectrum').
ref_m('profiles',     category, 'reprDataServiceAnalysis:SpectralAnalysis').

ref_m('PSD',     model, '/context_psd_workflow/navigate').
ref_m('PSD',     target_iframe, '/html/psd_workflow.html').
ref_m('PSD',     feature, 'fine_terrain').
ref_m('PSD',     category, 'realmSoil:SoilLayer').
ref_m('PSD',     category, 'propMass:Roughness').
ref_m('PSD',     category, 'reprDataServiceAnalysis:FourierTransform').
ref_m('PSD',     category, 'reprDataServiceAnalysis:PowerSpectrum').
ref_m('PSD',     category, 'reprDataServiceAnalysis:SpectralAnalysis').

ref_m('obstacles',     model, '/context_obstacles/navigate').
ref_m('obstacles',     target_iframe, '/html/obstacles.html').
ref_m('obstacles',     feature, 'fine_terrain').
ref_m('obstacles',     category, 'reprSpaceGeometry:Surface').

ref_m('autocorrelation',     model, '/context_autocorr/navigate').
ref_m('autocorrelation',     target_iframe, '/html/autocorr.html').
ref_m('autocorrelation',     feature, 'gross_terrain').
ref_m('autocorrelation',     category, 'propSpaceHeight:Topography').
ref_m('autocorrelation',     category, 'propFraction:Correlation').
ref_m('autocorrelation',     category, 'reprMathFunction:ProbabilityDensityFunction').
ref_m('autocorrelation',     category, 'propFraction:Probability').

ref_m('water',     model, '/context_water/navigate').
ref_m('water',     target_iframe, '/html/water.html').
ref_m('water',     feature, 'wave').
ref_m('water',     category, 'phenOcean:OceanPhenomena').
ref_m('water',     category, 'realm:Hydrosphere').
ref_m('water',     category, 'statePhysical:Droplet').
ref_m('water',     category, 'matrWater:WaterSubstance').

ref_m('seastate',     model, '/context_seastate/navigate').
ref_m('seastate',     target_iframe, '/html/seastate.html').
ref_m('seastate',     feature, 'wave').
ref_m('seastate',     category, 'phenWave:GravityWave').
ref_m('seastate',     category, 'propSpaceHeight:SeaState').
ref_m('seastate',     category, 'phenOcean:OceanPhenomena').

ref_m('lightning',     model, '/context_lightning/navigate').
ref_m('lightning',     target_iframe, '/html/lightning.html').
ref_m('lightning',     feature, 'clutter').
ref_m('lightning',     category, 'phenAtmoLightning:Lightning').

ref_m('electro-magnetic',     model, '/context_emi/navigate').
ref_m('electro-magnetic',     target_iframe, '/html/emi.html').
ref_m('electro-magnetic',     feature, 'clutter').
ref_m('electro-magnetic',     category, 'phenElecMag:ElectricField').
ref_m('electro-magnetic',     category, 'matrEquipment:Communications').

ref_m('clutter',     model, '/context_clutter/navigate').
ref_m('clutter',     target_iframe, '/html/clutter.html').
ref_m('clutter',     feature, 'clutter').
ref_m('clutter',     category, 'phenWaveNoise:Noise').
ref_m('clutter',     category, 'matrEquipment:Communications').

ref_m('fording',     model, '/context_fording/navigate').
ref_m('fording',     target_iframe, '/html/fording.html').
ref_m('fording',     feature, 'lakes').
ref_m('fording',     category, 'phenHydro:WaterFlow').

ref_m('lakes',     model, '/context_lakes/navigate').
ref_m('lakes',     target_iframe, '/html/lakes.html').
ref_m('lakes',     feature, 'lakes').
ref_m('lakes',     category, 'realmHydroBody:Lake').

ref_m('thermal',     model, '/context_thermal/navigate').
ref_m('thermal',     target_iframe, '/html/thermal.html').
ref_m('thermal',     feature, 'thermal').
ref_m('thermal',     category, 'phenMixing:Diffusion').
ref_m('thermal',     category, 'propConductivity:ThermalConductivity').
ref_m('thermal',     category, 'propDiffusivity:ThermalDiffusivity').
ref_m('thermal',     category, 'propTemperature:Temperature').
ref_m('thermal',     category, 'phenAtmo:Thermal').

ref_m('corrosion',     model, '/context_corrosion/navigate').
ref_m('corrosion',     target_iframe, '/html/corrosion.html').
ref_m('corrosion',     feature, 'corrosion').
ref_m('corrosion',     category, 'procChemical:Corrosion').
ref_m('corrosion',     category, 'procChemical:Oxidation').
ref_m('corrosion',     category, 'matrElement:Fe').
ref_m('corrosion',     category, 'phenSolid:Ablation').

ref_m('demos',     model, '/context_demos/navigate').
ref_m('demos',     target_iframe, '/html/demos.html').

ref_m('atmosphere',     model, '/context_atm/navigate').
ref_m('atmosphere',     model, '/context_standard_atmosphere/navigate').
ref_m('atmosphere',     target_iframe, '/html/atm.html').
ref_m('atmosphere',     target_iframe, '/html/standard_atmosphere.html').   % This will not get called, hidden
ref_m('atmosphere',     feature, 'thermal').
ref_m('atmosphere',     category, 'phenAtmo:Weather').
ref_m('atmosphere',     category, 'phenAtmoPressure:Barometric').
ref_m('atmosphere',     category, 'propFraction:Humidity').
ref_m('atmosphere',     category, 'propPressure:BarometricPressure').
ref_m('atmosphere',     category, 'phenAtmo:Thermal').
ref_m('atmosphere',     category, 'propTemperature:Temperature').

ref_m('solar',     model, '/context_solar/navigate').
ref_m('solar',     target_iframe, '/html/solar.html').
ref_m('solar',     feature, 'thermal').
ref_m('solar',     category, 'phenAtmo:Sunlight').
ref_m('solar',     category, 'phenElecMag:ElectricField').

ref_m('browse',     model, '/context_browse/navigate').
ref_m('browse',     target_iframe, '/html/browse.html').
ref_m('browse',     category, 'realm:Land').
ref_m('browse',     category, 'realm:Atmosphere').
ref_m('browse',     category, 'realm:Ocean').
ref_m('browse',     category, 'humanEnvirStandards:EnvironmentalStandard').
ref_m('browse',     category, 'reprDataService:DataService').
ref_m('browse',     category, 'phen:StochasticProcess').

ref_m('physical',     model, '/context_physical/navigate').
ref_m('physical',     target_iframe, '/html/physical.html').
ref_m('physical',     category, 'phenAtmo:Sunlight').
ref_m('physical',     category, 'humanEnvirStandards:EnvironmentalStandard').
ref_m('physical',     category, 'phenAtmo:Weather').
ref_m('physical',     category, 'repr:sciUnits').
ref_m('physical',     category, 'reprDataService:DataService').

ref_m('elements',     model, '/context_elements/navigate').
ref_m('elements',     target_iframe, '/html/elements.html').
ref_m('elements',     category, 'procChemical:Corrosion').

ref_m('materials',     model, '/context_materials/navigate').
ref_m('materials',     target_iframe, '/html/materials.html').
ref_m('materials',     category, 'procChemical:Corrosion').
ref_m('materials',     category, 'realmSoil:SoilLayer').

ref_(URL, feature, Feature) :-
    ref_m(ID, model, URL),
    ref_m(ID, feature, Feature), !.

ref_(URL, target_iframe, Page) :-
    ref_m(ID, model, URL),
    ref_m(ID, target_iframe, Page), !.

ref_(URL, target_iframe, Page) :-
    atom_concat('/context_browse/navigate?term=', Term, URL),
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
rdf_(clutter, ent:description, 'EMI and noise clutter models').
rdf_(lakes, ent:image, 'lake_sizes_icon.png').
rdf_(lakes, ent:description, 'Lake, river and inland water models').
rdf_(particles, ent:image, 'particle_sizes_icon.png').
rdf_(particles, ent:description, 'Particulate (droplets, aerosols, etc) models').
rdf_(thermal, ent:image, 'thermal_dispersion_icon.jpg').
rdf_(thermal, ent:description, 'Thermal, solar, and temperature models').
rdf_(rainfall, ent:image, 'rain_statistics_icon.png').
rdf_(rainfall, ent:description, 'Rainfall, cloud models').
rdf_(corrosion, ent:image, 'corrosion_oxidation_icon.jpg').
rdf_(corrosion, ent:description, 'Corrosion, oxidation models').

rdf_(require, ent:description, 'User specified context requirements.').
rdf_(require, ent:narrative, 'Project-specific requirements provide a means to connect practical applications to information available from an environmental context library. For example, a project requirement that states that a vehicle should be able to operate on terrain of a specific roughness, suggests a link to certain context models available from the server.  The links between the requirements and models are accomplished via semantic and ontological organization of the knowledge. In this case, certain keywords and phrases in a requirements document are tagged and allocated to specific environmental categories. This is aided by the application of ontologies such as SWEET (Semantic Web for Earth and Environmental Terminology from http://sweet.jpl.nasa.gov).').
rdf_(search, ent:description, 'Search the knowledgebase for models.').
rdf_(search, ent:narrative, 'Finding a model needed for a particular purpose is facilitated by various forms of search. A free-form search into the knowledgebase is provided by the search bar in the upper-right corner of the user-interface. This links to knowledge contained within the triple-store knowledgebase, largely independent of semantic context. Other more directed, semantically-driven searches are available from the search page. Links between specific categories of knowledge and models available within the server are contained here. To accomodate this, specific models are tagged and allocated to specific environmental categories. This is aided by the application of ontologies such as SWEET (Semantic Web for Earth and Environmental Terminology from http://sweet.jpl.nasa.gov).').
rdf_(browse, ent:description, 'Browse the knowledebase by category').
rdf_(browse, ent:narrative, 'Environmental and context knowledge follows a natural hierarchical organization. At the top level, we can break out the models into broad categories for Land, Atmosphere, and Aquatic. Below that  level, the specific models are allocated to more finely refined categoies such as terrain roughness. The basic hierarchy follows that of the SWEET ontology (Semantic Web for Earth and Environmental Terminology from http://sweet.jpl.nasa.gov).  ').
rdf_(workflow, ent:description, 'Workflows available to guide the model selection process').
rdf_(workflow, ent:narrative, 'A workflow is defined as a software-guided navigation to problem solving. A composable workflow allows for a sequence of problem solving steps depending on the knowledge available. Several workflows to access probability density function (PDF) environmental models and power spectral density (PSD) models are provided. More information on the concept of semantic and knowledge-based workflows is available from http://www.darpa.mil/WorkArea/DownloadAsset.aspx?id=2147485441.').
rdf_(map, ent:description, 'Navigate the atlas of localities which reference models.').
rdf_(map, ent:narrative, 'The scope of environmental models is world-wide. By their nature, environmental models depend on the particular characteristics of specific geospatial locations.  Where models have locality links we can search regions of interest to find what is available.').
rdf_(ref, ent:description, 'Search for references, citations, and specifications.').
rdf_(ref, ent:narrative, 'Each model has supporting documentation in the form of references and citations. We also distinguish between specifications, foundational research, requirements, and supporting material.  The Zotero citation management system is used to keep track of references and links and we use the SWEET ontology to tag the references with semantic environmental categories. For example, references relating to aquatic wave energy will get tagged with the phenWave:GravityWave class resource defined in SWEET.').
rdf_(resources, ent:description, 'Supplemental data resources for context models.').
rdf_(resources, ent:narrative, 'A supplemental knowledge-based system will provide semantic web discovery capability. The OSCAR (Ontological System for Context Artifacts and Resources) portal will guide discovery for users to find context models and associated metadata to enable their simulation.  The context models can include collections of PDFs and PSDs.  Context modeling resources include interactive links to tables and supporting doucments, such as environmental regulations, standards, specifications, and typical operational profiles.').
rdf_(features, ent:description, 'This home page contains environmental features along the top icon bar, and process steps as links.').
rdf_(features, ent:narrative, 'Home page contains environmental features along the top icon bar, and process steps as links along the vertical bulet list.').
rdf_(query, ent:description, 'Generic queries for SPARQL and Prolog.').
rdf_(query, ent:narrative, 'The knowledgebase has SPARQL and native Prolog query support.').
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
rdf_(atmospheric, child, corrosion).
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
rdf_(slopes, foundation, [a,10]).

rdf_(elevations, link, '/context_autocorr/navigate').
rdf_(elevations, comment, 'Model of terrain elevation difference.').
rdf_(elevations, foundation, [b,10]).

rdf_(profile, link, '/context_profile/navigate').
rdf_(profile, comment, 'Models of course and track profiles.').
rdf_(profile, foundation, [b,12]).

rdf_('man-made obstacles', link, '/context_obstacles/navigate').
rdf_('man-made obstacles', comment, 'Models of man-made obstacle profiles.').
rdf_('man-made obstacles', foundation, [d,12]).

rdf_('roughness', link, '/context_psd_workflow/navigate').
rdf_('roughness', comment, 'Models of courses from power spectral density (PSD) datasets.').
rdf_('roughness', foundation, [b,12]).

rdf_('rainfall amount', link, '/context_model/navigate?characteristics=rainfall&render=render').
rdf_('rainfall amount', target, target_iframe).
rdf_('rainfall amount', comment, 'Models of rainfall intensity').
rdf_('rainfall amount', foundation, [a,12]).

rdf_('lake size', link, '/context_model/navigate?characteristics=lakeSize&render=render').
rdf_('lake size', target, target_iframe).
rdf_('lake size', comment, 'Models of lake size distribution').
rdf_('lake size', foundation, [a,12]).

rdf_('wind speed', link, '/context_model/navigate?characteristics=windSpeed&render=render').
rdf_('wind speed', target, target_iframe).
rdf_('wind speed', comment, 'Models of wind speed').
rdf_('wind speed', foundation, [a,10]).

rdf_('wave height', link, '/context_model/navigate?characteristics=waveHeight&render=render').
rdf_('wave height', target, target_iframe).
rdf_('wave height', comment, 'Models of sea-state wave height distribution').
rdf_('wave height', foundation, [a,10]).

% wave frequency missing in triple store
rdf_('wave frequency', link, '/context_model/navigate?characteristics=waveFrequency&render=render').
rdf_('wave frequency', target, target_iframe).
rdf_('wave frequency', comment, 'Models of wave frequency distribution').
rdf_('wave frequency', foundation, [b,10]).

rdf_('particle size', link, '/context_model/navigate?characteristics=particleSizes&render=render').
rdf_('particle size', target, target_iframe).
rdf_('particle size', comment, 'Models of particle size distribution').
rdf_('particle size', foundation, [a,10]).

rdf_('EMI clutter', link, '/context_model/navigate?characteristics=clutterPower&render=render').
rdf_('EMI clutter', target, target_iframe).
rdf_('EMI clutter', comment, 'Models of clutter power distribution').
rdf_('EMI clutter', foundation, [a,10]).

rdf_('cloudiness', link, '/context_model/navigate?characteristics=cloudArea&render=render').
rdf_('cloudiness', target, target_iframe).
rdf_('cloudiness', comment, 'Models of cloud coverage').
rdf_('cloudiness', foundation, [a,10]).

rdf_('physical constants', link, '/context_resources/navigate').
rdf_('physical constants', comment, 'Properties of static environmental characteristics.').
rdf_('physical constants', foundation, [d,10]).

rdf_('requirements', link, '/context_require/navigate').
rdf_('requirements', comment, 'Navigate to source requirements.').
rdf_('requirements', foundation, [d,10]).

rdf_('thermal', link, '/context_thermal/navigate').
rdf_('thermal', comment, 'Model of thermal transients.').
rdf_('thermal', foundation, [c,10]).

rdf_('daily and seasonal', link, '/context_select/navigate?category=thermal').
rdf_('daily and seasonal', comment, 'Models and data for daily and seasonal temperatures').

rdf_('corrosion', link, '/context_corrosion/navigate').
rdf_('corrosion', comment, 'Model of corrosive growth.').
rdf_('corrosion', foundation, [c,8]).


% Fall-back info
rdf_(_, comment, '').
rdf_(_, link, '').
rdf_(_, target, '').
rdf_(_, foundation, '').

rdf_(a, foundation_doc, 'foundation/stochastic_analysis.pdf').
rdf_(b, foundation_doc, 'foundation/terrain_characterization.pdf').
rdf_(c, foundation_doc, 'foundation/diffusive_growth.pdf').
rdf_(d, foundation_doc, 'foundation/knowledge_based_environmental_modeling.pdf').



