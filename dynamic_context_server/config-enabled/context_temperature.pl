:- module(context_temperature, [
                                calculate_numeric_temperature/3
                               ]).

/** <module> Context temperature interface
    * Locale specific seasonal
    * Locale specific diurnal
*/


:- context:register(context_temperature:temperature).
:- context:register(context_temperature:navigate).
:- use_module(context_math).

% T(t) = T0+Ty*sin(2*pi/365*t+a)+(dT*sin(2*pi/365*t+b)+(Td-dT))*sin(2*pi*t+c)


calculate_symbolic_temperature(Site, T, S) :-
    rdfS(U, ent:name, Site),
    rdfR(U, ent:t0, T0),
    rdfR(U, ent:ty, Ty),
    rdfR(U, ent:dT, DT),
    rdfR(U, ent:td, Td),
    rdfR(U, ent:a, A),
    rdfR(U, ent:b, B),
    rdfR(U, ent:c, C),
    PI is pi,
    S = T0+Ty*sin(2*PI/365*T+A)+(DT*sin(2*PI/365*T+B)+(Td-DT))*sin(2*PI*T+C).

calculate_numeric_temperature(Site, T, Temperature) :-
    calculate_symbolic_temperature(Site, T, Symbolic_Temperature),
    Temperature is Symbolic_Temperature.

generate_yearly_temperature_profile(Site, Timespan, Profile) :-
   Timespan range [0,365]/0.1,
   Profile mapdot calculate_numeric_temperature(Site) ~> Timespan.

begin_c_skeleton('
#include <math.h>

float Get_Temperature (float time) {
   return ('
).

end_c_skeleton(');
}'
).

generate_c_code(Site, Code) :-
   calculate_symbolic_temperature(Site,time,ST),
   begin_c_skeleton(Inner),
   end_c_skeleton(Outer),
   context_codegen:generate_from_symbolic_skeleton(Inner, ST, Outer, Code).

temperature(Request) :-
   http_parameters(Request, [site(Site, [string])]),
   generate_yearly_temperature_profile(Site, TS, Profile),
   generate_c_code(Site, Code),
   reply_html_page(% cliopatria(default),
                   [title(Site),
                    %\(context_graphing:dygraph_script_load)
                    \(con_text:style)
                   ],
                   [h2('Interactive graph'),
                    \(context_graphing:dygraph_plot(false,
                                                    'Time, Temperature',
                                                    'day',
                                                    'temperature(C)',
                                                    ['Expected ', Site, ' temperature'],
                                                    [TS, Profile])),
                    br([]),
                    h3('The generated c-code:'),
                    pre(Code)
                   ]).


navigate -->
   html(
       \(con_text:table_form_target('Temperature profile >> ',
				    '/context_temperature/temperature',
				    target_iframe,
				    [['site', 'Wilmington']]
				   )
		     )
		   ).
