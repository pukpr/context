:- module(context_sweet_utils, []).

/** <module> SWEET utilities
    * Loading ontology
    * Configuring namespace
*/

:- use_module(user(user_db)).
:- use_module(components(messages)).

:- context:register(context_sweet_utils:import_sweet).
:- http_handler(root(terms), rdf_server, []).

%%   rdf_server(+Request)
%
%    This provides the terminology (OWL subclasses) for the dynamic context server
rdf_server(_) :-
    http_reply_file('./terms/defs.ttl', [mime_type('text/turtle')], []).

%%   sweet_uri(-Site)
%
%    Location of the SWEET terminology
sweet_uri('http://sweet.jpl.nasa.gov/2.3/sweetAll.owl').

% This is loaded and stored in the cache ahead of time
% see the file 020-prefixes.pl for registering namespaces

%%   load_standard(+All)
%
%    Load all terminology or minimal, default is minimal
load_standard(true) :-
   rdf_library:rdf_attach_library('terms/manifest.ttl'),
   rdf_library:rdf_load_library('standard-vocabulary'),
   rdf_library:rdf_attach_library('Components.ttl'),
   rdf_library:rdf_load_library('combined_ontology').

load_standard(false).

%%   load_sweet_remote 
%
%    Load SWEET off of remote site. Current default is to use local fast-loading database,
load_sweet_remote :-
   sweet_uri(SWEET),
   rdf_load(SWEET),
   findall(Files, rdf(SWEET,owl:imports,Files), SWEET_Files),
   !,
   maplist(rdf_load, SWEET_Files).

% load the ontologies in fresh from manifest
%%   import_sweet(+Request)
%
%    Import the SWEET ontology into the semantic knowledgebase
import_sweet(Request) :-
   http_parameters(Request, [all(All, [default(false)])]),
   authorized(write(default, clear)),

   call_showing_messages(
       (
   load_standard(All),
   rdf_library:rdf_attach_library('Manifest.ttl'),
   rdf_library:rdf_load_library('context'),
   process_sweets,
   % context_data_lib:populate_rdf_with_file_data,
   % load_sweet,
   print_message(informational, format('~w finished', 'ontology'))
       ),
       []).


%%   sweet_category(+Cat, -Description)
%
%    Descriptions of SWEET cataegories
sweet_category(human, 'human activity').
sweet_category(matr,  'matter description').
sweet_category(phen,  'phenomena (macroscale)').
sweet_category(proc,  'process (microscale)').
sweet_category(prop,  'property (observation)').
sweet_category(realm, 'realm of influence').
sweet_category(rela,  'relation (verb)').
sweet_category(repr,  'representation').
sweet_category(state, 'state (adjective,adverb)').

%%   prefix_each(F)
%
%     Used by *register_sweet*
prefix_each(F) :-
   rdf_split_url(_,Prefix,F),
   atomic_list_concat([Name,'owl'], '.', Prefix),
   sweet_category(Category, _),
   atom_concat(Category,_,Name),
   format(atom(Rule), ':- rdf_register_ns(~s,\'~s#\').', [Name,F]),
   print(Rule), nl.

% get all the prefixes and generate a listing of these for registering the 020-prefixes file
%%   register_sweet 
%
%    Find all namespaces that SWEET uses so thay can be registered by Prolog
register_sweet :-
   sweet_uri(SWEET),
   findall(Files, rdf(SWEET,owl:imports,Files), All),
   !,
   maplist(prefix_each, All),
   print('SWEET listing complete'), nl.

%%   belongs_to_sweet_category(+URI, -Prefix, -Category, -Description)
%
%    Query if belongs to a SWEET category
belongs_to_sweet_category(URI, Prefix, Category, Description) :-
   rdf_split_url(_,Prefix,URI),
   sweet_category(Category, Description),
   atom_concat(Category,_,Prefix).


%%   sweet_subject(-Name)
%
%    Returns a SWEET subject, use with findall
sweet_subject(Name) :-
   sweet_uri(SWEET),
   rdf(SWEET,owl:imports,F),
   atom_concat(F,'#',Pre),
   rdf(Subject, rdf:type, Sub),
   rdf_reachable(Sub, rdf:type, owl:'Class'),
   rdf_split_url(Pre, Term, Subject),
   concat(Pre, Term, Name).

%%   string_to_rdf(+String,-RDF, -NS, -Local)
%
%    Breakup string into global term and namespace:local
string_to_rdf(String,RDF, NS, Local) :-
   rdf_current_ns(NS, _),
   atomic_list_concat([NS,Local], ':', String),
   rdf_global_term(NS:Local, RDF).


%%   find_sweets(-Option)
%
%    Search elements for returning selection options
find_sweets(option([value(Out)],[NS, ' :: ', Local])) :-
   % rdf(_, dc:subject, literal(Out)),
   rdf(Name, ent:sweet, literal(Out)),
   string_to_rdf(Out, Name, NS, Local).
   %rdf_reachable(Name, rdf:type, owl:'Class').

%%   find_sweet_pairs(+Pair)
%
%    Returns string and global SWEET subjects
find_sweet_pairs([Name,Out]) :-
   rdf(_, dc:subject, literal(Out)),
   string_to_rdf(Out, Name, _NS, _Local),
   rdf_reachable(Name, rdf:type, owl:'Class').


%%   store_sweet(-Tuple)
%
%    Store a SWEET tuple
store_sweet([Name,Out]) :-
    not(rdf(Name, ent:sweet, literal(Out))),
    rdf_assert(Name, ent:sweet, literal(Out)), !.
store_sweet([_Name,_Out]).

%%   process_sweets 
%
%    Process individual SWEET classes
process_sweets :-
   setof(Sweet, Sweet ^ (find_sweet_pairs(Sweet)), Sweets),
   !,
   maplist(store_sweet, Sweets).


%%   print_all_sweet_subjects 
%
%    Print all SWEET subject names
print_all_sweet_subjects :-
   sweet_subject(Name),
   print(Name), nl,
   fail.



%%   save_sweet_db 
%
%    Save the SWEET database for quickkaccess
save_sweet_db :-
    sweet_uri(SWEET),
    rdf_load(SWEET),
    findall(Files, rdf(SWEET,owl:imports,Files), SWEET_Files),
    maplist(rdf_load, SWEET_Files),
    rdf_save_db('terms/sweet23_db.trp').

% rdf_retractall('http://sweet.jpl.nasa.gov/2.3/Arch', B, A).

