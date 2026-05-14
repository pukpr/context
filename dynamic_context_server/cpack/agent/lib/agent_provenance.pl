:- module(agent_provenance, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(lists)).
:- use_module(library(agent_api)).

:- rdf_register_prefix(dcterms, 'http://purl.org/dc/terms/').
:- rdf_register_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_register_prefix(agent, 'urn:agent:').

:- http_handler(root('agent/provenance'), provenance_graph, []).

provenance_graph(Request) :-
    catch(provenance_graph_(Request), Error, reply_provenance_error(Error)).

provenance_graph_(Request) :-
    http_parameters(Request, [graph(Graph, [])]),
    must_be_known_graph(Graph),
    graph_metadata_dict(Graph, Metadata),
    findall(
        _{
            subject: SubjectJson,
            predicate: PredicateJson,
            object: ObjectJson
         },
        provenance_triple(Graph, SubjectJson, PredicateJson, ObjectJson),
        Triples),
    reply_json_dict(_{
        status: "ok",
        graph: Graph,
        metadata: Metadata,
        provenance_triples: Triples
    }).

provenance_triple(Graph, SubjectJson, PredicateJson, ObjectJson) :-
    rdf(Subject, Predicate, Object, Graph),
    provenance_predicate(Predicate),
    resource_json(Subject, SubjectJson),
    resource_json(Predicate, PredicateJson),
    object_json(Object, ObjectJson).

provenance_predicate(Predicate) :-
    rdf_global_id(dcterms:_, Predicate),
    !.
provenance_predicate(Predicate) :-
    rdf_global_id(prov:_, Predicate),
    !.
provenance_predicate(Predicate) :-
    rdf_global_id(agent:_, Predicate).

must_be_known_graph(Graph) :-
    (   rdf_graph(Graph)
    ->  true
    ;   throw(error(existence_error(graph, Graph), _))
    ).

reply_provenance_error(Error) :-
    message_to_string(Error, Message),
    reply_json_dict(_{
        status: "error",
        error: Message
    }, [status(400)]).
