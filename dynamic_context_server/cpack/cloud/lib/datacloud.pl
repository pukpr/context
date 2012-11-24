/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Victor de Boer
    WWW:           http://www.few.vu.nl/~vbr240/
    Copyright (C): VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(datacloud,
	  [ write_cloud_graph/2,	% +Out, +Options
	    datacloud_link/4		% ?SourceSet, ?TargetSet, -Triple, +Opts
	  ]).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(debug)).

:- thread_local
	mg/1,
	graphset/2,
	linkcounter/4.

/** <module> Create a data-cloud image

Generates a datacloud based on the Manifest's  information in DOT, to be
visualized in graphviz. A cloud  node  is   defined  as  an entry in the
manifest files as used by   library(semweb/rdf_library)  that is labeled
using the type =|lib:CloudNode|= and consists of all the graphs that are
loaded (indirectly) through this entry.
*/

%% make_cloud_graph(+Out, +Options)
%
%	Generates a dot atom for loaded  manifest files. Options include
%
%	  * min_size(Float)
%	  * max_size(Float)
%	  Bounds for the node-sizes.  Defaults are 0.5 and 5.
%	  * unload_manifests(Boolean)
%	  If =true=, unload the Manifest files from the RDF store
%	  after processing.

write_cloud_graph(Out, Options):-
	make_manifests_graph,
	get_all_cn(CNList),
	get_sizes(CNList,CNSizeList,Options),
	debug(cloud, 'Collecting links ...', []),
	get_links(CNSizeList,Links,Options),
	debug(cloud, 'Create graph ...', []),
	format(Out, 'graph DataCloud {~n', []),
	make_dot(Out, CNSizeList,Links),
	format(Out, '}~n', []),
	clean_manifests_graph(Options).

make_dot(Out, CNSizeList,Links):-
	maplist(dot_node(Out), CNSizeList),
	maplist(dot_link(Out), Links).

dot_node(Out, cn(Short,Title,Triples,Size,_LOG)):-
	write_id(Out, Short),
	format(atom(ToolTip), '~w (~D triples)', [Title,Triples]),
	FontSize is 10+8*Size,
	write_attributes([ height(Size),
			   width(Size),
			   tooltip(ToolTip),
			   fontname('Helvetica-Bold'),
			   fontsize(FontSize)
			 ], Out),
	format(Out, ';~n', []).

dot_link(Out, cl(S,T,Size)):-
	format(atom(ToolTip), '~D links', [Size]),
	write_id(Out, S), write(Out, ' -- '), write_id(Out, T),
	write_attributes([edgetooltip(ToolTip)], Out),
	format(Out, ';~n', []).

% For each cloudnode, find the links to other cloudnodes. A link is
% cl(Source, Target, Strength)
%
get_links(CNSizeList,Out,_Options):-
	call_cleanup(( assert_graph_lookup(CNSizeList),
		       loop_over_triples,
		       findall(cl(A,B,C),linkcounter(_,A,B,C),Out)
		     ),
		     clean_links).

assert_graph_lookup([]).
assert_graph_lookup([cn(Short,_,_,_,LOG)|CNSizeList]):-
	forall(member(Graph,LOG),assert(graphset(Graph,Short))),
	assert_graph_lookup(CNSizeList).

loop_over_triples:-
	forall(cloud_link(GSS, GTS, _),
	       update_counter(GSS, GTS)).

%%	cloud_link(?SourceSet, ?TargetSet, -Triple) is nondet.
%
%	True if Triple relates  SourceSet   to  TargetSet. Defining what
%	constitutes a link is  far  from   trivial.  Currently  this  is
%	defined as a link  between  two   resources  that  are defned in
%	different graphsets, where `defined in' implies  that there is a
%	type property registered in the graphset. Also, bnodes can never
%	link two graphs because  you  cannot   point  to  them  from the
%	outside.

cloud_link(GSS, GTS, rdf(S,P,T)) :-
	rdf(S, P, T), atom(T),
	\+ rdf_is_bnode(S),
	\+ rdf_is_bnode(T),
	rdf(S, rdf:type, _ST, GS:_),
	rdf(T, rdf:type, TT, GT:_),
	GS \== GT,
	\+ (rdf(T, rdf:type, TT, GU:_),	% Hack to deal with VP loaded in
	    graphset(GU, GSS)),
	graphset(GS, GSS),		% multiple getty vocabularies
	graphset(GT, GTS),
	GSS \== GTS.


update_counter(Short1,Short2):-
	term_hash(Short1+Short2, Key),
	(   retract(linkcounter(Key,Short1,Short2,N))
	->  M is N+1
	;   M = 1
	),
	assert(linkcounter(Key,Short1,Short2,M)).

clean_links:-
	retractall(linkcounter(_,_,_,_)),
	retractall(graphset(_,_)).


% For each cloudnode, retrieve the size of the dot node. The Max and Min
% of a node size are given in the options through max_size/1 and
% min_size/1.
get_sizes(CNList,CNSizeList,Options):-
	option(max_size(MaxSize),Options, 3),
	option(min_size(MinSize),Options, 0.5),
	get_max_triples(CNList,MaxTriples),
	SizeFactor is sqrt(MaxTriples)/(MaxSize-MinSize),

	findall(cn(Short,Title,Triples,Size,LOG),
		(member(cn(Short,Title,Triples,LOG),CNList),
		 get_size(Triples, SizeFactor, MinSize, Size)),
		CNSizeList).

get_size(_, 0.0, MinSize, MinSize) :- !.
get_size(Triples, SizeFactor, MinSize, Size) :-
	Size is sqrt(Triples)/SizeFactor + MinSize.

get_max_triples(CNList,MaxTriples):-
	findall(X,member(cn(_,_,X,_LOG),CNList),TriplesList),
	max_list(TriplesList,MaxTriples).

get_all_cn(CNList):-
	findall(cn(Short,Title,Triples,ListOfGraphs),
		get_cloudnode(Short, Title, _Lib, ListOfGraphs, Triples),
		CNList),
	CNList \== [], !.
get_all_cn(_) :-
	existence_error(graph, 'lib:CloudNode').



% Make a graph of all the manifests that are loaded.
make_manifests_graph:-
	findall(Manifest,rdf_library_index(_ID,manifest(Manifest)),Manifests),
	sort(Manifests,SManifests),
	forall(member(OneMani,SManifests),
	       ( rdf_load(OneMani,[graph(ManifestGraph)]),
		 assert(mg(ManifestGraph))
	       )).

clean_manifests_graph(Options) :-
	option(unload_manifests(true), Options),
	forall(retract(mg(MG)),
	       rdf_unload(MG)).
clean_manifests_graph(_).


get_cloudnode(Short, Title, Lib, ListOfGraphs, Triples):-
	rdf(Lib, rdf:type, lib:'CloudNode'),
	rdf(Lib, dcterms:title, literal(TitleLit)),
	plain_text(TitleLit, Title),
	rdf_library_index(Short, source(Lib)),
	findall(G, library_graph(Short, G), ListOfGraphs),
	get_notriples(ListOfGraphs, Triples),
	debug(cloud, 'Cloud ~w (~w) ~D triples', [Short, Title, Triples]).

library_graph(Id, Graph) :-
	rdf_library_source(Id, Source),
	rdf_source(Graph, GraphSource),
	source_matches(GraphSource, Source).

source_matches(Source, Source) :- !.
source_matches(Plain, PlainGZ) :-
	file_name_extension(Plain, gz, PlainGZ).


plain_text(lang(_,Text), Text) :- !.
plain_text(Text, Text).

get_notriples(List, Size):-
	findall(Size1,
		(   member(G, List),
		    rdf_graph_property(G, triples(Size1))
		),
		NumList),!,
	sumlist(NumList, Size).


		 /*******************************
		 *	      DEBUG		*
		 *******************************/

%%	datacloud_link(?SourceSet, ?TargetSet, -Triple, +Options) is nondet.
%
%	True if Triple relates SourceSet to TargetSet.

datacloud_link(GSS, GTS, Triple, Options) :-
	make_manifests_graph,
	get_all_cn(CNList),
	get_sizes(CNList, CNSizeList, Options),
	setup_call_cleanup(
	    assert_graph_lookup(CNSizeList),
	    cloud_link(GSS, GTS, Triple),
	    retractall(graphset(_,_))).



		 /*******************************
		 *	    GRAPHVIZ UTIL	*
		 *******************************/

%%	write_id(+Id) is det.
%
%	Write a graphviz ID

write_id(Out, Id) :-
	sub_atom(Id, _, 1, _, C),
	\+ char_type(C, csym), !,
	c_escape(Id, String),
	format(Out, '"~s"', [String]).
write_id(Out, Id) :-
	write(Out, Id).

%%	write_attributes(+Attributes:list, +Out:stream) is det.
%
%	Write attribute values.  We define some special attributes:
%
%		* html(HTML)
%		Emit as label=<HTML>

write_attributes([], Out) :- !,
	format(Out, ' []').
write_attributes(List, Out) :- !,
	format(Out, ' [', []),
	write_attributes_2(List, Out),
	format(Out, ']', []).

write_attributes_2([], _).
write_attributes_2([H|T], Out) :-
	(   string_attribute(H)
	->  H =.. [Att, Value],
	    c_escape(Value, String),
	    format(Out, ' ~w="~s"', [Att, String])
	;   html_attribute(H, Att)
	->  arg(1, H, Value),
	    format(Out, ' ~w=<~s>', [Att, Value])
	;   H =.. [Name, Value],
	    format(Out, ' ~w=~w', [Name, Value])
	),
	write_attributes_2(T, Out).


string_attribute(label(_)).
string_attribute(url(_)).
string_attribute(href(_)).
string_attribute('URL'(_)).
string_attribute(fillcolor(_)).
string_attribute(style(_)).
string_attribute(tooltip(_)).
string_attribute(fontname(_)).
string_attribute(fontnames(_)).
string_attribute(edgetooltip(_)).

html_attribute(html(_), label).


c_escape(Atom, String) :-
	atom_codes(Atom, Codes),
	phrase(cstring(Codes), String).

%%	cstring(+Codes)//
%
%	Create a C-string. Normally =dot=  appears   to  be  using UTF-8
%	encoding. Would there be a  safer   way  to  transport non-ascii
%	characters, such as \uXXXX?

cstring([]) -->
	[].
cstring([H|T]) -->
	(   cchar(H)
	->  []
	;   [H]
	),
	cstring(T).

cchar(0'") --> "\\\"".
cchar(0'\n) --> "\\n".
cchar(0'\t) --> "\\t".
cchar(0'\b) --> "\\b".
