:- module(context_esker, [ element/2, criteria/2 ]).

:- use_module(context_math).

:- context:register(context_esker:navigate).
:- context:register(context_esker:plot).

configs(ballistics).
configs(swarm).
configs(ship).
configs(mbike).
configs(ramp).
configs(and).
configs(gwars).
configs(artillery).
configs(jeep).
configs(sudoku_mini).
configs(bct).
configs(ttt).
configs(bradley).
configs(vending).
configs(fc).
configs(ml).
configs(ground_vehicle).


navigate(Request) :-
   http_parameters(Request, [config(Name, [default(p)])]),
   Name \== p,
   atomic_list_concat(['examples/', Name, '-specs.pro'], Specs),
   atomic_list_concat(['examples/', Name, '-rules.pro'], Rules),
   consult(Specs),
   consult(Rules),
   % consult('examples/mbike-specs.pro'),
   % consult('examples/mbike-rules.pro'),
   % consult('config-enabled/ramp-specs.pro'),
   % consult('config-enabled/ramp-rules.pro'),
   findall([E,L], context_esker:element(E,L),List),
   findall([C,V], context_esker:criteria(C,V),CList),
   findall([C1,L1], context_esker:constraint(C1,L1),List1),
   reply_html_page(cliopatria(default),
                   [title('ESKER optimization')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Decision Set Optimization'),
                      h3('Criteria'),
                      \(context_esker:generate_eq(CList)),
		      br([]),
                      h3('Options'),
                      form([action(plot), target(target_iframe)],
			 [
                          table(\(context_esker:one_of_each_entry(List))),
                          br([]), hr([]), br([]),
			  input([type('submit'), name(kind), value('Submit')])
			 ]
                          ),
                      br([]),
                      table(\(context_esker:one_of_each_entry(List1))),
		      % table(\(con_text:table_input_pair(List1))),
                      br([]),
		      a(href('?'), 'home'),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).

navigate(Request) :-
   findall(option([value(Value)],[Value]), configs(Value), Names),
   % Names = [[mbike,mbike], [ramp,ramp]],
   reply_html_page(cliopatria(default),
                   [title('ESKER optimization')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Decision Set Optimization'),
                      h3('Criteria'),
                      form([action(navigate)],
			 [
			  select([name('config')], Names),
			  input([type('submit'), name(kind), value('submit')])
			 ]
                          ),
                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).


printCrit([_]) --> !.
printCrit([A|R]) -->
    {   A =.. [_Q, Attr, Val] },
    html([b(Attr), i(' = '), i(Val), br([])]),
    printCrit(R).

printDec([]) --> !.
printDec([A|R]) -->
    {
       % context_esker:element(E, ML),
       % member(A, ML)
       % make_atom(A,S)
    },
    html([b(E), i(' = '), \(con_text:flist(A)), br([])]),
    printDec(R).

/*
create_decisions([_],_,L,Last) :- reverse(L, Last), !.
create_decisions([Fun|L],[A|R], List, Ret) :-
    Fun =.. [_Q, _Attr, undecided], !,
    create_decisions(L, R, [A|List], Ret).
create_decisions([Fun|L],[_|R], List, Ret) :-
    Fun =.. [_Q, _Attr, F],
    create_decisions(L, R, [F|List], Ret).
*/

% splitter(A,L) :-  split_string(A, '*', "",L), length(L,N), N>1, !.
% splitter(A,A).


create_decisions([_],L,Last) :- reverse(L, Last), !.
create_decisions([Fun|L], List, Ret) :-
    Fun =.. [_Q, _Attr, undecided], !,
    create_decisions(L, [A|List], Ret).
create_decisions([Fun|L], List, Ret) :-
    Fun =.. [_Q, _Attr, F],
    term_string(T,F),
    create_decisions(L, [T|List], Ret).


plot(Request) :-
    http_parameters(Request, [], [form_data(Criteria)]),
    assert(max_value(-9000,empty_set)),
    assert(max_set([])),
    %create_decisions(Criteria, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], [], Decisions),
    % create_decisions(Criteria, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], [], Decisions),
    create_decisions(Criteria, [], Decisions),
    % print(user_error, Decisions),
    findall([C0,V], context_esker:criteria(C0,V),CList),
    % findall([E,L], context_esker:element(E,L),List),

    context_esker:optimize(Value, Decisions, CList, Opt),
    reply_html_page([title('Results'),
                     \(con_text:style)],
                    [
			h2('Constraining criteria selected'),
			\(printCrit(Criteria)),
			h2('Results'),
			p(['Value = ', Value]),
			\(printDec(Decisions))
                    ]
		  ).


%%	%%%%%%%%%%%%%%%%%%%
%
%

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Optimizer engine
%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic(max_value/2).
:- dynamic(stored/1).
:- dynamic(max_set/1).
:- dynamic(element/2).
:- dynamic(criteria/2).

max_value(-9000.0, empty_set).
max_set([]).

add_them_up([],[], Total, Total).
add_them_up([First|Rest],[[_,F]|R], Running, Total) :-
   Current is F*First+Running,
   add_them_up(Rest,R,Current,Total).

criteria_eq(List, Multipliers, Length, Total) :-
   add_them_up(List, Multipliers, 0.0, T),
   Total is T/Length.

one_of_each([],[]).
one_of_each([E=H|T],[H1|Ts]) :- member(H1,H), one_of_each(T,Ts).

sum_terms(_, _, [],Total,Total).
sum_terms(Goal, List, [Item|Rest],Sum,T) :-
   % call_with_args(Goal, _, Item, List, P), % this is a gprolog meta call, same as the following 2
   % G=..[Goal, Item, List, P],
   G=..[Goal, _, Item, List, P],
   print(user_error, G),
   call(G),
   Total is Sum+P,
   sum_terms(Goal, List, Rest, Total, T).

sum_all_terms([], _, L, L).
sum_all_terms([[First,_]|Rest], Set, L, Summed_List) :-
   sum_terms(First, Set, Set, 0.0, Result),
   sum_all_terms(Rest, Set, [Result|L], Summed_List).

check_maximum(T, Set) :-
   max_value(Total,Current),
   T > Total,
   retract(max_value(Total,Current)),
   asserta(max_value(T,Set)).

store_set(List) :-
   max_set(L),
   retract(max_set(L)),
   asserta(max_set(List)).

merge([], [], L, L).
merge([F1|R1], [F2|R2], L, R) :-
    merge(R1, R2, [F1=F2|L], R).

merge3([], [], [], L, L).
merge3([F1|R1], [F2|R2], [[_,K]|R3], L, R) :-
    merge3(R1, R2, R3, [F1=F2*K|L], R).

% build_list(_,[], L, L).
% build_list(E,[V|Rest], L, Final) :-
%    build_list(E,Rest, [E=V|L], Final).

optimize(_, Set, Eq, _) :-
   length(Eq,Length),
   findall(E=V, context_esker:element(E,V), All),
   print(user_error, All),
   one_of_each(All, Set),
   print(user_error, Set),
   sum_all_terms(Eq, Set, [], Summed_List),
   reverse(Summed_List, SL),
   criteria_eq(SL, Eq, Length, T),
   check_maximum(T, Set),
   %% Store complete set so that we can view it on the way out
   store_set(SL),
   fail.
optimize(Value, Which, Eq, Opt) :-
   max_value(Value, Which),
   findall(C, context_esker:criteria(C,_), Crit),
   max_set(Opts),
   % Merge 3 with eq measure, multiply out the values
   merge3(Crit,Opts,Eq,[],ROpt),
   reverse(ROpt,Opt).

% Which = [SUSPENSION,26*inch,titanium, etc]
% Eq = [weight*1, reliability*1, etc]
%% Tabling, Memoization, Cacheing

check_tabled(Goal) :- stored(Goal).
store_tabled(Goal) :- assertz(stored(Goal)).


%% XML generate rules

pro2xml(Term,W):-
	Term=..List,
	xml_convert(List,W).

make_atom([],'').
make_atom(A,A) :- atom(A),!.
make_atom(A,B) :- atomic(A),number_atom(A,B).

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

%% CGI generate rules

cgi_concat(Final,[],Final).
cgi_concat(S,[A=B|R],Final) :-
   concat([S,A,'=',B,'&'],Atom),
   cgi_concat(Atom,R,Final).
cgi_concat(List,Result) :-
   cgi_concat("",List,Result), !.

%% utility membership  rules

one_of(One, Of_Group, From_List) :-
   element(Of_Group, Any),
   member(One, Any),
   member(One, From_List).
one_of(One, Of_Group) :-
   element(Of_Group, Any),
   member(One, Any).

%%% Reading XML from a file or a string / No attributes allowed

:- dynamic(xml_s/1).
xml_s([]).  % This is where the XML string is kept

get_xml_char(C) :- xml_s([]), get(C).
get_xml_char(C) :-
   xml_s([C|Last]),
   retract(xml_s([C|Last])),
   asserta(xml_s(Last)).
get_xml_char_any(C) :- xml_s([]),  get0(C).
get_xml_char_any(C) :- get_xml_char(C).

read_xml_from_string(S, E) :- retract(xml_s([])), asserta(xml_s(S)), get_xml_char(C), get_item(C,E,_),!.

read_xml_from_file(File, E) :- see(File), get_xml_char(C), get_item(C,E,_), seen, !.

get_item(C,Element, " ") :-
   C is "<",
   get_xml_char(C2),
   open_close(C2,Element).
get_item(C,Element, C1) :-
   tag_name(C,El,C1),
   name(Element, El).

open_close(C,Element) :-
   (C is "!";C is "?"),!,
   read_to(C,">"),
   get_xml_char(C2),
   !,
   get_item(C2,Element,_).
% Deal with possible / after < (the close variant)
open_close(C,close_marker(El)) :-
   C is "/", !,
   get_xml_char(C1),
   tag_name(C1,El,C2),
   read_to(C2,">").
% Otherwise read and return whole element (the open variant)
open_close(C,[Word|Cont]) :-
   tag_name(C,El,C1),
   name(Word, El),
   C1 is ">", !,
   get_xml_char(C3),
   get_item(C3, Rest, Leader),
   rest_items(Leader, Rest, Cont).

% Given the last char C, read any additional chars to get to the character C1
read_to(C,C1) :- C is C1, !.
read_to(_,C1) :- get_xml_char_any(C), read_to(C,C1).

% Read a tag, given its first char and also returning the char after the name
tag_name(C2,[],C2) :-
   (C2 is " "; C2 is "<"; C2 is ">"), !.
tag_name(C2,[C2|L],C3) :-
   get_xml_char_any(C),
   tag_name(C,L,C3).

% Process rest of items, given the last one read.
rest_items(_, close_marker(_), []) :- !.
rest_items(L, E,[E|Es]) :-
   L is "<",
   get_item(L,E2,L1),!,
   rest_items(L1, E2,Es).
rest_items(_, E,[E|Es]) :-
   get_xml_char(C),
   get_item(C,E2,L),
   rest_items(L, E2,Es).

search_xml([[Tag|X]|_], Tag, X):-!.
search_xml([F|_], Tag, X) :-
   search_xml(F, Tag, X), !.
search_xml([_|R], Tag, X) :-
   search_xml(R, Tag, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generation of Test data or Rule placeholders
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_rule([F|R]) :-
   print(F),
   print_rule(R).
print_rule([]) :-
   nl.

one_of_each_rule :-
   criteria(Crit,_),
   element(El, List),
   member(Alternative, List),
   random(0,10,Num),
   print_rule([Crit,'(',El,',',Alternative,',_,X) :- X is ', Num, '.']),
   fail.

generate_random_rules :-
  set_seed(1),
  !,
  one_of_each_rule.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generation of HTML form entries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rewrite atom into lower case
upcase_atom(Atom, AtomUpper) :-
			atom_chars(Atom, Chars),
			% upcase_codes(Chars, CharsUpper),
			% atom_chars(AtomUpper, CharsUpper)
			atom_chars(AtomUpper, Chars).
/*
upcase_codes([],[]).
upcase_codes([F|R], [FL|RL]) :-
	lower_upper(F,FL),
	upcase_codes(R,RL).
*/

generate_eq([]) --> !.
generate_eq([[Crit|Weight]|R]) -->
   html([
    b(' +'), b(Weight), i('×'), i(Crit)
   ]),
   generate_eq(R).

one_rb(_,[]) --> !.
one_rb(N,[F|R]) -->
    {
        with_output_to(atom(A), write(F))
    },
    html([
        input([type('radio'),
               name(N),
               value( A)],
              b(A))
         ]
        ),
    one_rb(N,R).

one_of_each_entry([]) --> !.
one_of_each_entry([[El,L]|R]) -->
   html(tr([
	    td([width('50%'),valign(top)], b(El)),
	    td([width('50%'),valign(top)],
	         [input([type('radio'),
                  name(El),
                  value( undecided),
		  checked(true)],
                  b('?')),
		   \(one_rb(El,L))])
	    ]
	  )
       ),
   one_of_each_entry(R).


read_json(B) :-
   open('E2E-demo-constraints-v1.json', read,F),
   json_read(F,json([_A=json(L)])),
   member(B,L) .
