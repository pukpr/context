:- module(context_materials, [
			    ]).

:- context:register(context_materials:navigate).
:- context:register(context_materials:plot).

navigate(Request) :-

   reply_html_page(cliopatria(default),
                   [title('Materials Properties')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Materials Properties Resources'),
                      ul([
                          li([\(con_text:gif(resources)),
                              a([href('/context_soil/soil_table'),
                                target('target_iframe')],
                                'Soil table')
                             ]
                            ),
                          li([\(con_text:gif(resources)),
                              a([href('/context_corrosion/corrosion_scale_table'),
                                target('target_iframe')],
                                'Corrosion scale table')
                             ]
                            ),
                          li([\(con_text:gif(resources)),
                              a([href('/context_friction/coefficient_friction_table'),
                                target('target_iframe')],
                                'Coefficients of friction table')
                             ]
                            )
                         ]),
                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).

