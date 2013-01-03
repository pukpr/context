:- module(context_elements, [
			    ]).

/** <module> Periodic table of elements
    * Semantically created table
    *
*/

:- use_module(context_math).

:- context:register(context_elements:navigate).
:- context:register(context_elements:table).
:- context:register(context_elements:property).

%%   element(-Option)
%
%    Search elements for returning selection options
element(option([label(Name), value(El)],[Name])) :-
   rdfS(El, pt:name, Name),
   rdfI(El, pt:atomicNumber, _).

%%   navigate(+Request)
%
%    Dynamic page to periodic table of elements
navigate(Request) :-
   findall(El, element(El), Elements),
   sort(Elements, Els),
   reply_html_page(cliopatria(default),
                   [title('Periodic Table of the Elements'),
		    \(con_text:style_submit)],

                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Periodic Table of the elements'),
                      p(abbr([title='element pick from drop-down menu'],'element')),
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


%%   registry(+ID, -Link)
%
%    Link to a registry for element
registry(ID, a([href(U),target(target_iframe)],
	       [img([src('/html/images/ref.gif'),
		     title(Title)]),
			   ID])) :-
    % rdf(pt:casRegistryID, rdfs:seeAlso, U),
    atom_concat('http://dbpedia.org/page/',ID, U),
    rdfS(pt:casRegistryID, rdfs:comment, Title).

%%   property(+Request)
%
%    Dynamic page to periodic table of elements properties
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
      rdfE(El, pt:classification, Class),
      rdfE(El, pt:standardState, State),
      rdf(El, pt:period, PeriodName),
      rdfI(PeriodName, pt:number, Period),
      rdfE(El, pt:block, Block),
      registry(RegID, Link),
      reply_html_page([title('Periodic Table'),
                     \(con_text:style)],
                    [
                       p([b(Name), ' : ', i(Color)]),

		     \(con_text:table_entries(
				    [
				    ['symbol', Sym],
				    ['atomic number', AtNum],
				    ['atomic weight', AtWeight],
				    ['period', Period],
				    ['classification', Class],
				    ['standard state', State],
				    ['block', Block],
				    ['CAS registry ID', Link]
				    ]
					     )
		      )
		    ]
		  )
    ;
      context_elements:table(Request)
    ).

%%   row(+Row, -Period)
%
%    PTOE row
row(Row, Period) :-
   rdf_global_term(Period, P),
   findall(N-Col, (rdf(Col, pt:period, P),
		   rdfI(Col, pt:atomicNumber, N)), R),
   keysort(R, Row).

%%   spacer(+N, -Spacer)
%
%    Make table spacer
spacer(N, td([colspan(N)],[])).

%%   span(+El, -T)
%
%    Special spanning elemants
span('He', T) :- spacer(30,T), !.
span('B',  T) :- spacer(24,T), !.
span('Al', T) :- spacer(24,T), !.
span('Sc', T) :- spacer(14,T), !.
span('Y',  T) :- spacer(14,T), !.
span(_, '').

%%   cell_color(+Family, -Color)
%
%    Color cell according to element family
cell_color('Actinoid', 'Wheat').
cell_color('Lanthanoid', 'LightGray').
cell_color('Noble gas', 'SeaShell').
cell_color('Coinage metal', 'Silver').
cell_color('Halogen', 'Cyan'). %
cell_color('Pnictogen', 'LightSteelBlue'). %
cell_color('Chalcogen', 'PeachPuff'). %
cell_color('Alkali metal', 'SkyBlue').
cell_color('Alkaline earth metal', 'Aquamarine'). %
cell_color('Lanthanoid', 'LightGray').
cell_color(_, 'GhostWhite').

% group_color

cell_color(El, Name, Color) :-
    rdf_global_term(El, E),
    rdf(E, pt:group, Group),
    rdfS(Group, pt:name, Name),
    cell_color(Name, Color), !.
cell_color(_, 'others', 'GhostWhite').

cellColor([]) --> !.
cellColor([Group|Rest]) -->
    {
     rdf_global_term(Group, G),
     rdfS(G, pt:name, Name),
     cell_color(Name, Color)
    },
    html(tr([td([bgcolor=Color],Name)])), !,
    cellColor(Rest).
cellColor([_|Rest]) --> !,
    cellColor(Rest).

%%   periodic_row(+U,+R)//
%
%    Row of table
periodic_row(_,[]) --> !.
periodic_row(El, [_N-E|R]) -->
   {
      rdfS(E, pt:symbol, S),
      rdfS(E, pt:name, Name),
      span(S, CS),
      (	  El = E ->
	  Str = big(b(S)),
          Color = 'Yellow'
      ;
          Str = small(S),
          cell_color(E, _Name, Color)
      ),
      uri_encoded(query_value, E, URI),
      format(atom(H), 'property?kind=property&element=~w', [URI]),
      HREF = a([href(H),target(render)], Str)
   },
   html([CS,td([title=Name,bgcolor=Color], small(HREF))]),
   periodic_row(El, R).

%%   periodic_table(+U,+T)//
%
%    Complete inline table
periodic_table(_,[]) --> !.
periodic_table(El, [F|R]) -->
   html(tr(\(periodic_row(El,F)))),
   periodic_table(El, R).

%%   table(+Request)
%
%    Render the periodic table of elements
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
    findall(Group, rdf(Group, rdf:type, pt:'Group'), Groups),
    reply_html_page([title('Periodic Table') ],
                    [
		      p(['Highlighting ', b(Name), ' : ', b(Symbol)]),
		      table( \(periodic_table(El, List)) ),
                      br([]),
                      table([ \(cellColor(Groups))])

                    ]
		  ).







