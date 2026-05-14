:- module(conf_agent, []).

:- cpack_register(agent,
                  'cpack/agent',
                  [ home_url('https://github.com/pukpr/context'),
                    requires([])
                  ]).

:- use_module(applications(app_agent)).
:- use_module(cliopatria(hooks)).

cliopatria:menu_item(admin/'/agent', 'Agent').
