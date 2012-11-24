:- module(context_resources, []).

/** <module> Context interface to resources and artifacts
   * Selects various models from categories
*/

:- use_module(context).
:- use_module(context_math).

:- context:register(context_resources:navigate).


navigate(Request) :-
    % http_parameters(Request, [category(Cat, [default(others)])]),
    reply_html_page(
	cliopatria(default),
	[title('Resources')],
        [
         \(con_text:table_with_iframe_target(
                        Request,
                        [h1('Common resources and artifacts for context modeling'),
                         ul([
                             li([\(con_text:gif(resources)),
                                 \(con_text:alert(['Placeholder to ', b('OSCAR')], 'Not linked yet'))
                                ]),
                             li([\(con_text:gif(resources)),
                                 a(href('/context_climate_AR7038/navigate'),
                                   'Access environmental resource artifacts')
                                ]
                               ),
                             li([\(con_text:gif(resources)),
                                 a(href('/context_atm/navigate'),
                                   'Access standard atmosphere resource artifacts')
                                ]
                               ),
                             li([\(con_text:gif(resources)),
                                 a(href('/context_solar/navigate'),
                                   'Access standard solar artifacts')
                                ]
                               )
                            ])
                        ]
                                            )
          )
          ]
          ).


