:- module(context_elements, [
			    ]).

:- use_module(context_math).

:- context:register(context_elements:navigate).
:- context:register(context_elements:table).
:- context:register(context_elements:property).

element(option([value(El)],[Name])) :-
   rdfS(El, pt:name, Name),
   rdfI(El, pt:atomicNumber, _).

navigate(Request) :-
   findall(El, element(El), Els),
   reply_html_page(cliopatria(default),
                   [title('Periodic Table of the Elements'),
		    \(con_text:style_submit)],

                   [\(con_text:table_with_iframe_target(
                                    Request,
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

% rdfQ(El, _, El).

/*
PeriodicTable:casRegistryID
	a owl:DatatypeProperty ;
	rdfs:comment "Chemical Abstracts Service (CAS) related information" ;
	rdfs:seeAlso <http://www.cas.org> .
*/

registry(ID, a([href(U),target(target_iframe)],
	       [img([src('/html/images/ref.gif'),
		     title(Title)]),
			   ID])) :-
    % rdf(pt:casRegistryID, rdfs:seeAlso, U),
    atom_concat('http://dbpedia.org/page/',ID, U),
    rdfS(pt:casRegistryID, rdfs:comment, Title).

property(Request) :-
    http_parameters(Request, [element(El, []),
			      kind(Kind, [])]),
    rdfS(El, pt:name, Name),
    (
    Kind = property ->
      rdfI(El, pt:atomicNumber, AtNum),
      rdfS(El, pt:casRegistryID, RegID),
      rdfR(El, pt:atomicWeight, AtWeight),
      rdfS(El, pt:symbol, Sym),
      rdfS(El, pt:color, Color),
      registry(RegID, Link),
      reply_html_page([title('Periodic Table'),
                     \(con_text:style)],
                    [
                       p([Name, ' : ', Color]),

		     \(con_text:table_entries(
				    [
				    ['symbol', Sym],
				    ['atomic number', AtNum],
				    ['atomic weight', AtWeight],
				    ['CAS registry ID', Link]
				    ]
					     )

		      )

/*
	PeriodicTable:atomicNumber 27 ;
	PeriodicTable:atomicWeight "58.933200"^^xsd:float ;
	PeriodicTable:block PeriodicTable:d-block ;
	PeriodicTable:casRegistryID "7440-48-4"^^xsd:string ;
	PeriodicTable:classification PeriodicTable:Metallic ;
	PeriodicTable:color "lustrous, metallic, greyish tinge"^^xsd:string ;
	PeriodicTable:group PeriodicTable:group_9 ;
	PeriodicTable:name "cobalt"^^xsd:string ;
	PeriodicTable:period PeriodicTable:period_4 ;
	PeriodicTable:standardState PeriodicTable:solid ;
	PeriodicTable:symbol "Co"^^xsd:string .
*/
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

periodic_row(_,[]) --> !.
periodic_row(El, [_N-E|R]) -->
   {
      rdfS(E, pt:symbol, S),
      rdfS(E, pt:name, Name),
      span(S, CS),
      (	  El = E ->
	  Str = big(b(S))
      ;
          Str = small(S)
      ),
      uri_encoded(query_value, E, URI),
      format(atom(H), 'property?kind=property&element=~w', [URI]),
      HREF = a([href(H),target(render)], Str)
   },
   html([CS,td([title=Name], small(HREF))]),
   periodic_row(El, R).

periodic_table(_,[]) --> !.
periodic_table(El, [F|R]) -->
   html(tr(\(periodic_row(El,F)))),
   periodic_table(El, R).


table(Request) :-
    http_parameters(Request, [element(El, [])]),
    rdfS(El, pt:name, Name),
    rdfS(El, pt:symbol, Symbol),
    row(One, pt:period_1),
    row(Two, pt:period_2),
    row(Three, pt:period_3),
    row(Four, pt:period_4),
    row(Five, pt:period_5),
    row(Six, pt:period_6),
    row(Seven, pt:period_7),
    List = [One,Two,Three,Four,Five,Six,Seven],
    % print(user_error, List),
    reply_html_page([title('Periodic Table'),
                     \(con_text:style)],
                    [
		      p(['Highlighting ', b(Name), ' : ', b(Symbol)]),
		      table( \(periodic_table(El, List)) )
                    ]
		  ).

