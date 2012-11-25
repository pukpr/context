:- module(context_physical, [

			    ]).

:- use_module(context_math).

:- context:register(context_physical:navigate).

find_entries(Class, List) :-
   findall([Attr,Val], (rdf_(Class, Attr, Term),
			Term \= '',
			with_output_to(atom(Val), write(Term))),
			List).

find_units(List) :-
   findall([Attr,Val], (rdf_(Attr, ent:units, UID),
			rdf_(UID, ent:unit, Term),
			with_output_to(atom(Val), write(Term))),
			List).

navigate(Request) :-
	/*
   findall([Attr1,Val1], rdf_(ent:standardAtmosphere, Attr1, Val1), Atm),
   findall([Attr2,Val2], rdf_(ent:water, Attr2, Val2), Water),
   findall([Attr3,Val3], rdf_(ent:solar, Attr3, Val3), Solar),
   findall([Attr4,Val4], rdf_(ent:solar, Attr4, Val4), PhysConst),
   */
   find_entries(ent:standardAtmosphere, Atm),
   find_entries(ent:water, Water),
   find_entries(ent:solar, Solar),
   find_entries(ent:physicalConstants, PhysConst),
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


