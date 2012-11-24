% XML generate rules

xml(Prolog) :-
    % html_set_options([dialect(xhtml)]),
    html(Prolog, A, []),
    maplist(print, A).

test_1_xml :-
    xml( a(  [b(  [c(1),c(2)],[] ),  d( [e(1),e(2)],[] )] )).

test_2_xml :-
    xml( a(  [b(  [c=1,c=2],[] ),  d( [e=1,e=2],[] )] )).

/*
pro2xml(Term,W):-
	Term=..List,
	xml_convert(List,W).

make_atom([],'').
make_atom(A,A) :- atom(A),!.
make_atom(A,B) :- atomic(A),atom_number(A,B).

concat(Final,[],Final).
concat(S,[F|R],Final) :-
   make_atom(S,S1),
   make_atom(F,F1),
   atom_concat(S1, F1, Atom),!,
   concat(Atom,R,Final).
concat(List,Result) :-
   concat("",List,Result), !.

xml_convert([[]],''):-!.
xml_convert(String,   String):- atomic(String),!.
xml_convert([String], String):- atomic(String),!.
xml_convert([X],W):-!,
	X=..List,
	xml_convert(List,W).
xml_convert([Tag|Rl],W):-
	xml_content(Rl,W1),
   concat(['<',Tag,'>',W1,'</',Tag,'>'],W).

xml_content([],  '').
xml_content([X],  Y):- make_atom(X,Y),!.
xml_content([X|R],W):-
	X=..ListX,
	xml_convert(ListX,W0),
	xml_content(R,W1),
   concat([W0,W1],W).

*/
