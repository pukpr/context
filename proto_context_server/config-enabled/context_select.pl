:- module(context_select, []).

/** <module> Context Selection interface
   * Selects various models from categories
*/

:- use_module(context).
:- use_module(context_math).

:- context:register(context_select:navigate).

ref_search(Term, Description) -->
    html(p([\(con_text:gif(ref)),
	    a([href('/context_ref_search/search_sweet?name='+Term),
	       target=target_iframe], Description)
	   ])
	).

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

icon_bar(Highlight) -->
    {
     findall(Icon, icon_cell_link(Highlight, Icon), Icons)
    },
    html(table([border(0)],
               [tr(Icons)])).

dispatch(lakes) -->
	html([h1('Inland water models'),
	      p(a([href('/context_model/navigate?characteristics=lakeSize&render=render'),
		   target(target_iframe)], 'Lake size PDF models')),
	      p(a([href='/context_fording/navigate'],'Fording model')),
	      \(ref_search('realmHydroBody:Lake', 'Lake references')),
	      p(a([href('/context_lakes/navigate')],
		  [img(src('/html/images/example.gif')),
		   'Example seasonal lake ice data']))
	     ]
	    ).

dispatch(corrosion) -->
	html([h1('Corrosion models'),
	       p(a([href('/context_corrosion/navigate')], 'Corrosion models')),
	      \(ref_search('procChemical:Corrosion', 'Corrosion references')),
	       p(a([href('/context_corrosion/corrosion_scale_table'),
		    target=render], 'Corrosion table'))
	     ]
	    ).

dispatch(fine_terrain) -->
	html([h1('Fine terrain models'),
              p(a([href('/context_psd_workflow/navigate')], 'PSD workflow')),
              p(a([href('/context_obstacles/navigate')], 'Obstacle workflow')),
	      p(a([href('/context_profile/navigate')], 'Recently profiled terrains')),
	      \(ref_search('realmBiolBiome:Terrain', 'Terrain references')),
	      p(a([href='/context_soil/soil_table',
		    target=target_iframe],'Soil classification table')),
	      \(ref_search('realmSoil:SoilLayer', 'Soil references')),
              p(a([href('/context_friction/coefficient_friction_table'),
                   target=target_iframe], 'Coefficient of Friction table')),
	      \(ref_search('propMass:Roughness', 'Friction references'))

	     ]
	    ).

dispatch(gross_terrain) -->
	html([h1('Gross terrain (topographic) models'),
	      p(a([href('/context_autocorr/navigate')], 'Terrain elevation correlation')),
	      p(a([href('/context_model/navigate?characteristics=slopes&render=render'),
		   target(target_iframe)], 'Slope PDF models')),
	      p([ img(src('/html/images/example.jpg')),
		  \(context_pdf:example)
		]),
	      \(ref_search('propSpaceHeight:Topography', 'Topography references'))
	     ]
	    ).

dispatch(wind) -->
	html([h1('Wind models'),
	      p(a([href('/context_model/navigate?characteristics=windSpeed&render=render'),
		   target(target_iframe)], 'Wind PDF models')),
	      \(ref_search('phenAtmoWind:Wind', 'Wind references'))
	     ]
	    ).

dispatch(wave) -->
	html([h1('Aquatic wave models'),
	      p(a([href('/context_model/navigate?characteristics=waveHeight&render=render'),
		   target(target_iframe)], 'Aquatic wave PDF models')),
	      p(a([href='/context_water/chart',
		    target=target_iframe],'Water properties')),
	      p(a([href='/context_water/navigate'],'Water buoyancy')),
	      p(a([href='/context_seastate/seastate_table',
		    target=target_iframe],'Sea-state ranges')),
	      p(a([href('/context_seastate/navigate')], 'Wave/sea state model')), % own page
	      \(ref_search('phenWave:GravityWave', 'Wave references'))
	     ]
	    ).

dispatch(thermal) -->
	html([h1('Thermal and temperature models'),
	      p(a([href='/context_thermal/navigate'],'Thermal diffusion model')),
	      p([ img(src('/html/images/example.jpg')),
		   \(context_temperature:navigate)]
	       ),
	      \(ref_search('propConductivity:ThermalConductivity', 'Thermal conductivity references')),
	      \(ref_search('propTemperature:Temperature', 'Temperature references'))
	     ]
	    ).

dispatch(particles) -->
	html([h1('Particle and aerosol models'),
	      p(a([href('/context_model/navigate?characteristics=particleSizes&render=render'),
		   target(target_iframe)], 'Particle size PDF models')),
	      \(ref_search('matrAerosol:Particulate', 'Particle references'))
	     ]
	    ).

dispatch(clutter) -->
	html([h1('EMI and clutter models'),
	      p(a([href='/context_lightning/navigate'],'Lightning indirect effects model')),
	      p(a([href='/context_clutter/navigate'],'Clutter integrate model')),
	      p(a([href('/context_model/navigate?characteristics=clutterPower&render=render'),
		   target(target_iframe)], 'EMI clutter pdf models')),
	      \(ref_search('phenElecMag:ElectricField', 'EMI references')),
	      \(ref_search('phenAtmoLightning:Lightning', 'Lightning references'))
	     ]
	    ).

dispatch(rainfall) -->
	html([h1('Rainfall models'),
	      p(a([href('/context_model/navigate?characteristics=rainfall&render=render'),
		   target(target_iframe)], 'Rainfall PDF models')),
	      p(a([href('/context_model/navigate?characteristics=cloudArea&render=render'),
		   target(target_iframe)], 'Cloud PDF models')),
	      \(ref_search('phenAtmoPrecipitation:Rainfall', 'Rainfall references'))
	     ]
	    ).

dispatch(others) -->
	html([h1('other models'),
	      []
	     ]
	    ).

navigate(Request) :-
    http_parameters(Request, [category(Cat, [default(others)])]),
    reply_html_page(
	cliopatria(default),
	[title('PDF')],
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
