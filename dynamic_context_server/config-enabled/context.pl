:- module(context, [rdfR/3,
		    rdfI/3,
		    rdfS/3,
		    rdfL/3,
		    rdfV/4,
		    rdfx/3,
		    rdfx_collect/1,
		    rdfx_clean/0]).

/** <module> Top-level context module
    * Registering callbacks
    * Common utilities
    *
*/

:- use_module(library(http/http_host)).
:- use_module(library(http/http_wrapper)).

:- rdf_meta
   rdfx(o,o,o),
   rdfR(o,o,?),
   rdfI(o,o,?),
   rdfS(o,o,?),
   rdfL(o,o,?),
   rdfV(o,o,?,?).


ac_result([Obj,Type], json([ label=Type,
                              type=Obj,
                              href='javascript:location.reload(false);'
                            ])).

find_term(Request) :-
    http_parameters(Request,
                    [ query(Query, [description('Typed string')]),
                      maxResultsDisplayed(Max, [integer, default(100), description('Max number of results to show') ])
                    ]),
    print(user_error, ['Q', Query]),
    atom_to_term(Query, (Call=String), []),
    atom_codes(Atom, String),
    findall([URI,Type], call(Call, Atom, URI, Type), Completions0),
    sort(Completions0, Completions1),
    length(Completions1, Count),
    con_text:first_n(Max, Completions1, Completions2),
    maplist(ac_result, Completions2, Completions),
    reply_json(json([ query = json([ count=Count ]),
                      results = Completions
                    ])).


ref_link_to_pdf(FileName, Dest, Link) :-
   format(atom(Link), '/ref/~s#nameddest=~s', [FileName,Dest]).
page_link_to_pdf(FileName, Page, Link) :-
   format(atom(Link), '/ref/~s#page=~d', [FileName,Page]).

strip_numbers([], Input, Final) :- reverse(Input, Final).
strip_numbers([literal(type(_, Str))| Rest], Input, Final) :-
    atom_number(Str, Num),
    strip_numbers(Rest, [Num|Input], Final).

cgi_pairs([[Key,Value]], Input, Output) :-
    atomic_list_concat([Input, Key, '=', Value], Output).
cgi_pairs([[Key,Value]|R], Input, Output) :-
    atomic_list_concat([Input, Key, '=', Value, '&'], Next),
    !, cgi_pairs(R, Next, Output).

encode_cgi(Action, CGI_List, URL) :-
    atomic_list_concat([Action, '?'], Next),
    cgi_pairs(CGI_List, Next, URL).

encode_service(Hostname, Port, Cmd, Service) :-
    atomic_list_concat(['http://', Hostname, ':', Port, Cmd], Service).

create_url(Request, Service, CGIs, URL) :-
    http_current_host(Request, Hostname, Port, [global(true)]),
    % print(user_error, [Hostname, Port]),
    encode_service(Hostname, Port, Service, Serv),
    encode_cgi(Serv, CGIs, URL).

referer(N) :-
      http_current_request(Request),
      member(referer(URL), Request),
      parse_url(URL, P),
      member(path(Name), P),
      (   member(search([Key=Val]), P) ->
          atomic_list_concat([Name,'/',Key,'/',Val], N)
      ;
          N = Name
      ).

holder(Request,URI) :-
      member(request_uri(URI), Request), !.
holder(_Request,aaa).


replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

replace_chars(This, That, Input, Output) :-
   write_to_chars(Input,Chars),
   atom_codes(This, [C]),
   atom_codes(That, [S]),
   replace(C, S, Chars, String),
   atom_to_chars(Output, String).

replace_word(Old, New, Orig, Replaced) :-
    atomic_list_concat(Split, Old, Orig),
    atomic_list_concat(Split, New, Replaced), !.

/*
max_list([], C, C).
max_list([H|T], C, M) :- C2 is max(C, H), max_list(T, C2, M).
max_list([H|T], M) :- max_list(T, H, M).
*/

max_min([], Max, Min, Max, Min).
max_min([H|T], Max, Min, Mx, Mn) :-
    CX is max(Max, H),
    CN is min(Min, H),
    max_min(T, CX, CN, Mx, Mn).
max_min([H|T], Max, Min) :- max_min(T, H, H, Max, Min).

uri_index_link(URI, Link) :-
   uri_encoded(path, URI, U),
   Link = a(href('/browse/list_resource?r='+U),'[link]').
uri_index_link(URI, Title, Link) :-
   uri_encoded(path, URI, U),
   Link = a(href('/browse/list_resource?r='+U),Title).
uri_index_link(URI, Title, Target, Link) :-
   uri_encoded(path, URI, U),
   Link = a([href('/browse/list_resource?r='+U),
	    target(Target)],Title).

/** register(-ModuleHandler:string) is det
 *
 * register a URL handler.
 * The name of the HTTP parameter and CB handler are the same
 * with : switched with /
 */
register(ModuleHandler) :-
   % replace module delimiter : with REST slash /
   replace_chars(':', '/', ModuleHandler, RestCB),
   http_handler(root(RestCB), ModuleHandler, []).

/*
ac_hook(ModuleHandler) :-
   % replace module delimiter : with REST slash /
   replace_chars(':', '/', ModuleHandler, RestCB),
   http_handler(RestCB, ModuleHandler, []).
*/

create_global_term(Literal, Term) :-
   atom(Literal),
   rdf_litindex:rdf_tokenize_literal(Literal,[W1,W2]),
   rdf_global_term(W1:W2, Term).
create_global_term(Literal, Term) :-
   concise_term(Term, Literal).
/*   var(Literal),
   rdf_global_term(W1:W2, Term),
   rdf_current_ns(NS, W1),
   .*/

find_and_break_out_terms(Atom, Start, Length, Begin, Middle, Last) :-
    string_to_atom(S, Atom),
    sub_string(S, 0, Start, _A1, Begin),
    N1 is Start,
    sub_string(S, N1, Length, Aft, Middle),
    N2 is N1 + Length,
    sub_string(S, N2, Aft, _A3, Last).
find_and_break_out_terms(Subtext, Text, Begin, Middle, End) :-
   sub_string(Text, Start, Length, _After, Subtext),
   find_and_break_out_terms(Text, Start, Length, Begin, Middle, End).

rdf_default(Subj, Pred, Obj, _Default) :-
   rdf_global_term(Pred, P),
   rdf(Subj, P, literal(Obj)), !.
rdf_default(_, _, Default, Default).


% Storing RDF triples

storeRDF(A, E, B, C) :-
    is_list(C),
    with_output_to(atom(S), write(C)),
    rdf_global_term(E:B, BB),
    not(rdf(A,BB,_)),
    rdf_assert(A,BB,literal(S)), !.
storeRDF(A, E, B, C) :-
    rdf_global_term(E:B, BB),
    not(rdf(A,BB,_)),
    rdf_assert(A,BB,literal(C)), !.
storeRDF(A, _E, B, C) :-
    is_list(C),
    print(user_error, [skippedList, A,B,'\n']), !.
storeRDF(A, _E, B, C) :-
    print(user_error, [skipped, A,B,C, '\n']).

prefix('ent').

make_name(Name, Ent, E) :-
    prefix(Ent),
    rdf_global_term(Ent:Name, E0),
    uri_normalized(E0, E).

% RDF utilities

rdfR(Subj, Pred, Obj) :-
    (
     rdf(Subj, Pred, literal(type(xsd:decimal, Val)))
    ;
     rdf(Subj, Pred, literal(type(xsd:double, Val)))
    ;
     rdf(Subj, Pred, literal(type(xsd:float, Val)))
    ),
    atom_number(Val, Obj).
rdfI(Subj, Pred, Obj) :-
    (
     rdf(Subj, Pred, literal(type(xsd:integer, Val)))
    ;
     rdf(Subj, Pred, literal(type(xsd:int, Val)))
    ),
    atom_number(Val, Obj).
rdfL(Subj, Pred, Obj) :-
    rdf(Subj, Pred, literal(Val)),
    atom_to_term(Val, Obj, _).
rdfV(Subj, Pred, Obj, Vars) :-
    rdf(Subj, Pred, literal(Val)),
    atom_to_term(Val, Obj, Vars).
rdfS(Subj, Pred, Obj) :-
    (
       rdf(Subj, Pred, literal(type(xsd:string, Obj)))
    ;
       rdf(Subj, Pred, literal(Obj)),
       atom(Obj)
    ).


:- dynamic(rdf_temp/3).

concise_term(URI, PrefixNotation) :-
    rdf_global_id(Prefix:Local, URI),
    atomic_list_concat([Prefix, :, Local], PrefixNotation).

rdf_store(A, B, C) :-
    % print(user_error, [A, B, C, '\n']),
    not(rdf_temp(A,B,C)), % Don't store duplicates
    !,
    asserta(rdf_temp(A,B,C)).
rdf_store(_A, _B, _C).

rdfx_collect(List) :-
    findall(rdf(A,B,C), rdf_temp(A,B,C), List).

rdfx_clean :-
    retractall(rdf_temp(_,_,_)).

check_rdf(literal(type(_,A)), Atom) :-
    atomic_list_concat(['"', A, '"'], Atom).
check_rdf(literal(A), Atom) :-
    atomic_list_concat(['"', A, '"'], Atom).
check_rdf(Term, Short) :-
    concise_term(Term, Short).


rdfx(Subj, Pred, Value) :-
    (
    rdf(Subj, Pred, literal(type(xsd:decimal, Val))),
	atom_number(Val, Value);
    rdf(Subj, Pred, literal(type(xsd:double, Val))),
	atom_number(Val, Value);
    rdf(Subj, Pred, literal(type(xsd:int, Val))),
	atom_number(Val, Value);
    rdf(Subj, Pred, literal(type(xsd:integer, Val))),
	atom_number(Val, Value);
    rdf(Subj, Pred, literal(Value))
    ),
    check_rdf(Subj, S),
    check_rdf(Pred, P),
    check_rdf(literal(Value), O),
    rdf_store(S,P,O).
rdfx(Subj, Pred, Obj) :-
    rdf(Subj, Pred, Obj),
    check_rdf(Subj, S),
    check_rdf(Pred, P),
    check_rdf(Obj, O),
    rdf_store(S,P,O).

:- dynamic(local_stored_rdf_graph_list/1).

display_directed_graph(_) :-
    rdfx_collect(List),
    % retract(local_stored_rdf_graph_list(_)),
    asserta(local_stored_rdf_graph_list(List)),
    reply_html_page(title('Directed Graph Plot'),
                    [
                     \graphviz_graph(local_stored_rdf_graph_list,
                                    [graph_attributes([rankdir('LR')])])
                    ]),
    rdfx_clean.
%    retract(local_stored_rdf_graph_list(List)).

:- register(context:display_directed_graph).
:- register(context:find_term).

:- http_handler('/context/find_term', find_term, []).

