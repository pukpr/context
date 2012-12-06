:- module(context_elements, [
			    ]).

:- use_module(context_math).

:- context:register(context_elements:navigate).
:- context:register(context_elements:table).
:- context:register(context_elements:property).

element(option([value(El)],[Name])) :-
   rdfS(El, pt:name, Name),
   rdfI(El, pt:atomicNumber, _).

navigate(_Request) :-
   findall(El, element(El), Els),
   reply_html_page(cliopatria(default),
                   [title('Periodic Table of the Elements'),
		    \(con_text:style_submit)],

                   [\(con_text:table_with_iframe_target(
                                    target_iframe,
		     [
                      h1('Periodic Table of the elements'),
                      p(''),
                      form([action(property), target(target_iframe)],
			 [
			  select([name('element')], Els),
			  input([type('submit'), name(kind), value('table'),
				% onclick(\(con_text:target_iframe))
				onclick('subm(this.form,"target_iframe");')
				]),
			  input([type('submit'), name(kind), value('property'),
				 onclick('subm(this.form,"render");')
				 % onclick(\(con_text:target_render))
				])
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).
property(Request) :-
    http_parameters(Request, [element(El, []),
			      kind(Kind, [])]),
    rdfS(El, pt:name, Name),
    (
    Kind = property ->
    reply_html_page([title('Periodic Table'),
                     \(con_text:style)],
                    [
                       p(Name)
                    ]
		  )
    ;
    context_elements:table(Request)
    ).

row(Row, Period) :-
   rdf_global_term(Period, P),
   % rdfI(P, pt:number, N),
   findall(N-Col, (rdf(Col, pt:period, P),
		   rdfI(Col, pt:atomicNumber, N)), R),
   keysort(R, Row).

spacer(N, td([colspan(N)],[])).

span('He', T) :- spacer(30,T), !.
span('B',  T) :- spacer(24,T), !.
span('Al', T) :- spacer(24,T), !.
span('Sc', T) :- spacer(14,T), !.
span('Y',  T) :- spacer(14,T), !.
span(_, '').

periodic_row([]) --> !.
periodic_row([_N-El|R]) -->
   {
      rdfS(El, pt:symbol, S),
      span(S, CS)
   },
   html([CS,td(small(small(S)))]),
   periodic_row(R).

periodic_table([]) --> !.
periodic_table([F|R]) -->
   html(tr(
	    \(periodic_row(F))
	  )
       ),
   periodic_table(R).


table(Request) :-
    http_parameters(Request, [element(El, [])]),
    row(One, pt:period_1),
    row(Two, pt:period_2),
    row(Three, pt:period_3),
    row(Four, pt:period_4),
    row(Five, pt:period_5),
    row(Six, pt:period_6),
    row(Seven, pt:period_7),
    List = [One,Two,Three,Four,Five,Six,Seven],
    print(user_error, List),
    reply_html_page([title('Periodic Table'),
                     \(con_text:style)],
                    [
		      p(b(El)),
		      table(
			  \(periodic_table(List))
			   )
                    ]
		  ).

