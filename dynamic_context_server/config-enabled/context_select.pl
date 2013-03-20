:- module(context_select, []).

/** <module> Context Selection interface
   * Selects various models from categories
*/

:- use_module(context).
:- use_module(context_math).

:- context:register(context_select:navigate).

%%   ref_search(+Term, +Description)//
%
%    List inline refs that match term
ref_search(Term, Description) -->
    html(p([\(con_text:gif(ref)),
	    a([href('/context_ref_search/search_sweet?name='+Term),
	       target=target_iframe], Description)
	   ])
	).


%%   g(+Type, +Contents)//
%
%    Create a GIF inline icon
g(Type, Contents) -->
    html(p([
	     \(con_text:gif(Type)),
	     Contents
	   ]
	  )
	).


%%   icon_cell_link(+Highlight, -Td)
%
%    Create a cell link to feature
icon_cell_link(Highlight,
               td(a(href(Select+Obj),
                    img([height(Size),src(Path+Icon),
                         title(Description)])))) :-
        rdf_(Obj, ent:image, Icon),
        rdf_(Obj, ent:description, Description),
        Select='/context_select/navigate?category=',
        Path='/html/static_pages/gems/',
        (   Highlight=Obj ->
            Size=60
        ;
            Size=40
        ).

%%   icon_bar(+Highlight)//
%
%    Generate an icon bar of feature categories, highlighting currently selected
icon_bar(Highlight) -->
    {
     findall(Icon, icon_cell_link(Highlight, Icon), Icons)
    },
    html(table([border(0)],
               [tr(Icons)])).

%%   dispatch(+Feature_Category)//
%
%    Generate an inline list of available models corresponding to feature category
dispatch(lakes) -->
	html([h1('Inland water models'),
	      \g(search,
		 a([href('/context_model/navigate?characteristics=lakeSize'),
		   target(target_iframe)], 'Lake size PDF models')),
	      \g(example,
		 a([href='/context_fording/navigate'],'Fording model')),
	      \(ref_search('realmHydroBody:Lake', 'Lake references')),
	      \g(example,
		 a([href('/context_lakes/navigate')],
		  'Example seasonal lake ice data')),
              \g(browse,
		  a([href('/context_browse/navigate?term=aquatic')],
                  'Browse aquatic characteristics')
	       )
	     ]
	    ).

dispatch(corrosion) -->
	html([h1('Corrosion models'),
	      \g(workflow,
		  a([href('/context_corrosion/navigate')], 'Corrosion models')),
	      \(ref_search('procChemical:Corrosion', 'Corrosion references')),
	      \g(resources,
		 a([href('/context_corrosion/corrosion_scale_table'),
		    target=render], 'Corrosion table')),
              \g(browse,
		  a([href('/context_browse/navigate?term=context')],
                  'Browse environmental characteristics')
	       )

             ]
	    ).

dispatch(fine_terrain) -->
	html([h1('Fine terrain models'),
              \g(workflow,
		 a([href('/context_psd_workflow/navigate')], 'PSD workflow')),
              \g(workflow,
		 a([href('/context_obstacles/navigate')], 'Obstacle workflow')),
	      \g(workflow,
		 a([href('/context_profile/navigate')], 'Semi-Markov Model profiled terrains')),
	      \ref_search('realmBiolBiome:Terrain', 'Terrain references'),
	      \g(resources,
		 a([href='/context_soil/soil_table',
		    target=target_iframe],'Soil classification table')),
	      \ref_search('realmSoil:SoilLayer', 'Soil references'),
              \g(resources,
		  a([href('/context_friction/coefficient_friction_table'),
                   target=target_iframe], 'Coefficient of Friction table')
	       ),
	      \(ref_search('propMass:Roughness', 'Friction references')),
              \g(browse,
		  a([href('/context_browse/navigate?term=fine terrain')],
                  'Browse fine terrain characteristics')
	       )

	     ]
	    ).

dispatch(gross_terrain) -->
	html([h1('Gross terrain (topographic) models'),
	      \g(workflow,
		 a([href('/context_autocorr/navigate')], 'Terrain elevation correlation')),
	      \g(search,
		 a([href('/context_model/navigate?characteristics=slopes'),
		   target(target_iframe)], 'Slope PDF models')),
	      \g(example,
		 \(context_pdf:example)
		),
	      \(ref_search('propSpaceHeight:Topography', 'Topography references')),
              \g(browse,
		  a([href('/context_browse/navigate?term=gross terrain')],
                  'Browse gross terrain characteristics')
	       )
	     ]
	    ).

dispatch(wind) -->
	html([h1('Wind models'),
	      \g(search,
		 a([href('/context_model/navigate?characteristics=windSpeed'),
		   target(target_iframe)], 'Wind PDF models')),
	      \(ref_search('phenAtmoWind:Wind', 'Wind references')),
              \g(browse,
		  a([href('/context_browse/navigate?term=atmospheric')],
                  'Browse atmospheric characteristics')
	       )
	     ]
	    ).

dispatch(wave) -->
	html([h1('Aquatic wave models'),
	      \g(search,
		 a([href('/context_model/navigate?characteristics=waveHeight'),
		   target(target_iframe)], 'Aquatic wave height PDF models')),
	      \g(search,
		 a([href('/context_model/navigate?characteristics=waveFrequency'),
		   target(target_iframe)], 'Aquatic wave frequency PDF models')),
	      \g(workflow,
		 a([href('/context_seastate/navigate')], 'Wave/sea state model')), % own page
	      \g(resources,
		 a([href='/context_seastate/seastate_table',
		    target=target_iframe],'Sea-state ranges')),
	      \g(example,
		 a([href='/context_water/navigate'],'Water buoyancy')),
	      \(ref_search('phenWave:GravityWave', 'Wave references')),
              \g(browse,
		  a([href('/context_browse/navigate?term=aquatic')],
                  'Browse aquatic characteristics')
	       ),
	      \g(resources,
		 span([i('Fresh water '),
		    a([href='/context_water/chart',
		    target=target_iframe],'density'),
		 \(con_text:radio_box_two('viscosity table',
				       '/context_water/viscosity',
				       'target_iframe',
				       'kind',
				       ['dynamic', 'dynamic'],
				       ['kinematic', 'kinematic']))
		   ])
		)

	     ]
	    ).

dispatch(thermal) -->
	html([h1('Thermal and temperature models'),
	      \g(workflow,
                 i(['Locations with monthly temperature profiles',
                    \(context_temperature:navigate)
                   ])
                ),
	      \g(example,
		 i(['detailed for Baltimore and Wilmington',
                  \(context_temperature:example)
                 ])),
	      \g(example,
		 a([href='/context_thermal/navigate'],'Thermal diffusion model')),
	      \(ref_search('propConductivity:ThermalConductivity', 'Thermal conductivity references')),
	      \(ref_search('propTemperature:Temperature', 'Temperature references')),
              \g(browse,
		  a([href('/context_browse/navigate?term=context')],
                  'Browse environmental characteristics')
	       )
	     ]
	    ).

dispatch(particles) -->
	html([h1('Particle and aerosol models'),
	      \g(search,
		 a([href('/context_model/navigate?characteristics=particleSizes'),
		   target(target_iframe)], 'Particle size PDF models')),
	      \(ref_search('matrAerosol:Particulate', 'Particle references')),
              \g(browse,
		  a([href('/context_browse/navigate?term=atmospheric')],
                  'Browse atmospheric characteristics')
	       )
	     ]
	    ).

dispatch(clutter) -->
	html([h1('EMI and clutter models'),
	      \g(resources,
		 a([href='/context_emi/navigate'],'EMI resources')),
	      \g(resources,
		 a([href='/context_lightning/navigate'],'Lightning indirect effects model')),
	      \g(example,
		 a([href='/context_clutter/navigate'],'Clutter integrate model')),
	      \g(search,
		 a([href('/context_model/navigate?characteristics=clutterPower'),
		   target(target_iframe)], 'EMI clutter pdf models')),
	      \(ref_search('phenElecMag:ElectricField', 'EMI references')),
	      \(ref_search('phenAtmoLightning:Lightning', 'Lightning references')),
              \g(browse,
		  a([href('/context_browse/navigate?term=EMI')],
                  'Browse EMI characteristics')
	       )
	     ]
	    ).

dispatch(rainfall) -->
	html([h1('Rainfall models'),
	      \g(search,
		 a([href('/context_model/navigate?characteristics=rainfall'),
		   target(target_iframe)], 'Rainfall PDF models')),
	      \g(search,
		 a([href('/context_model/navigate?characteristics=cloudArea'),
		   target(target_iframe)], 'Cloud PDF models')),
	      \g(workflow,
                 i(['Locations with monthly precipitation profiles',
                    \(context_weather:navigate)
                   ])
                ),
	      \(ref_search('phenAtmoPrecipitation:Rainfall', 'Rainfall references')),
              \g(browse,
		  a([href('/context_browse/navigate?term=atmospheric')],
                  'Browse atmospheric characteristics')
	       )
	     ]
	    ).

dispatch(reservoir) -->
	html([h1('Fossil fuel resource models'),
	      \g(workflow,
                  a([href('/context_shock_model/navigate')], 'Oil Shock model')),
	      \g(workflow,
                  a([href('/context_oil_fields/navigate')], 'Reservoirs of the world')),
	      \g(workflow,
                  a([href('/context_co2/navigate')], 'Carbon Emissions and Climate Sensitivity')),
	      \g(workflow,
                  a([href('/context_demos/climate_ou')], 'Climate O-U Random Walk')),
	      \g(workflow,
                  a([href('/context_diffusive_decline/navigate')], 'Diffusive Decline'))
	     ]
	    ).

dispatch(others) -->
	html([h1('other models'),
	      []
	     ]
	    ).

%%   navigate(+Request)
%
%    Dynamic page to feature selection
navigate(Request) :-
    http_parameters(Request, [category(Cat, [default(others)])]),
    reply_html_page(
	cliopatria(default),
	[title('Feature Selection')],
	\(con_text:table_with_iframe_target(
                       Request,
                       [
                        \(context_select:icon_bar(Cat)),
                        \(context_select:dispatch(Cat)),
                        br([]),
                        br([]),
                        \(con_text:render_iframe(render))
                       ]
					   )
	 )
		   ).
