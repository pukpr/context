:- module(agent_api,
          [ agent_controlled_graph/1,
            agent_graph_category/2,
            graph_summary_dict/2,
            graph_metadata_dict/2,
            resource_json/2,
            object_json/2
          ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(library(debug)).
:- use_module(library(lists)).

:- rdf_register_prefix(agent, 'urn:agent:').
:- rdf_register_prefix(dcterms, 'http://purl.org/dc/terms/').
:- rdf_register_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#').

:- http_handler(root(agent),                    agent_home,        []).
:- http_handler(root('agent/status'),           agent_status,      []).
:- http_handler(root('agent/load_proposal'),    load_proposal,     [method(post)]).
:- http_handler(root('agent/unload_proposal'),  unload_proposal,   [method(post)]).
:- http_handler(root('agent/validate_graph'),   validate_graph,    []).
:- http_handler(root('agent/diff'),             diff_graph,        []).

agent_home(_Request) :-
    status_dict(Status),
    reply_html_page(
        cliopatria(default),
        title('Agent control'),
        [ h1('Agent control'),
          p('Small agent overlay for loading, validating, diffing, and removing proposal graphs.'),
          h2('Status'),
          pre(\json_text(Status)),
          h2('Endpoints'),
          ul([ li(code('GET /agent/status')),
               li(code('POST /agent/load_proposal')),
               li(code('POST /agent/unload_proposal')),
               li(code('GET /agent/validate_graph?graph=<graph-uri>')),
               li(code('GET /agent/diff?graph=<graph-uri>')),
               li(code('GET /agent/provenance?graph=<graph-uri>'))
             ])
        ]).

json_text(Dict) -->
    { with_output_to(string(Text), json_write_dict(current_output, Dict, [width(0)])) },
    html(Text).

agent_status(_Request) :-
    status_dict(Status),
    reply_json_dict(Status).

status_dict(_{
    service: "agentic-control",
    loaded: true,
    cpack: "agent",
    graph_counts: _{
        proposal: ProposalCount,
        staging: StagingCount,
        accepted: AcceptedCount,
        controlled: ControlledCount,
        total: TotalGraphs
    },
    endpoints: [
        "/agent/status",
        "/agent/load_proposal",
        "/agent/unload_proposal",
        "/agent/validate_graph",
        "/agent/diff",
        "/agent/provenance"
    ]
}) :-
    aggregate_all(count, rdf_graph(_), TotalGraphs),
    aggregate_all(count, nonempty_agent_graph(proposal), ProposalCount),
    aggregate_all(count, nonempty_agent_graph(staging), StagingCount),
    aggregate_all(count, nonempty_agent_graph(accepted), AcceptedCount),
    aggregate_all(count, (rdf_graph(Graph), agent_controlled_graph(Graph), nonempty_graph(Graph)), ControlledCount).

load_proposal(Request) :-
    api_reply(load_proposal_(Request)).

load_proposal_(Request) :-
    read_load_payload(Request, Payload),
    proposal_graph(Payload, Graph, Stage, RunId),
    _{turtle:Turtle} :< Payload,
    must_be_nonempty_text(Turtle, turtle),
    ensure_allowed_stage(Stage),
    with_mutex(agent_graph_update,
               rdf_transaction(do_load_proposal(Graph, Stage, RunId, Payload, Turtle))),
    graph_summary_dict(Graph, Summary),
    reply_json_dict(Summary.put(_{
        status: "loaded",
        run_id: RunId,
        stage: Stage
    }), [status(201)]).

do_load_proposal(Graph, Stage, RunId, Payload, Turtle) :-
    ensure_graph_is_new(Graph),
    open_string(Turtle, Stream),
    call_cleanup(
        rdf_load(stream(Stream), [graph(Graph), format(turtle)]),
        close(Stream)),
    assert_graph_metadata(Graph, Stage, RunId, Payload).

unload_proposal(Request) :-
    api_reply(unload_proposal_(Request)).

unload_proposal_(Request) :-
    graph_request(Request, Graph),
    must_be_controlled_graph(Graph),
    forbid_accepted_graph(Graph),
    graph_summary_dict(Graph, Summary),
    with_mutex(agent_graph_update,
               rdf_transaction(rdf_unload_graph(Graph))),
    reply_json_dict(_{
        status: "unloaded",
        graph: Graph,
        removed_triples: Summary.triples
    }).

validate_graph(Request) :-
    api_reply(validate_graph_(Request)).

validate_graph_(Request) :-
    graph_request(Request, Graph),
    validation_dict(Graph, Validation),
    reply_json_dict(Validation).

diff_graph(Request) :-
    api_reply(diff_graph_(Request)).

diff_graph_(Request) :-
    http_parameters(Request,
                    [ graph(Graph, []),
                      limit(Limit, [integer, default(200)])
                    ]),
    must_be_existing_graph(Graph),
    graph_metadata_dict(Graph, Provenance),
    findall(Triple,
            unique_graph_triple(Graph, Triple, Provenance),
            Triples0),
    length(Triples0, Total),
    first_n(Limit, Triples0, Triples),
    reply_json_dict(_{
        status: "ok",
        graph: Graph,
        unique_triple_count: Total,
        returned_triples: Triples
    }).

unique_graph_triple(Graph, Triple, Provenance) :-
    rdf(Subject, Predicate, Object, Graph),
    \+ (( rdf(Subject, Predicate, Object, OtherGraph),
          OtherGraph \== Graph )),
    Triple = _{
        subject: SubjectJson,
        predicate: PredicateJson,
        object: ObjectJson,
        graph: Graph,
        provenance: Provenance
    },
    resource_json(Subject, SubjectJson),
    resource_json(Predicate, PredicateJson),
    object_json(Object, ObjectJson).

validation_dict(Graph,
                _{ status: Status,
                   graph: Graph,
                   category: Category,
                   triples: TripleCount,
                   metadata: MetadataChecks,
                   errors: Errors,
                   warnings: Warnings
                 }) :-
    (   rdf_graph(Graph)
    ->  Category0 = existing
    ;   Category0 = missing
    ),
    (   Category0 == missing
    ->  TripleCount = 0,
        Category = none,
        Errors = ["Graph does not exist"],
        Warnings = [],
        MetadataChecks = _{
            controlled_graph: false,
            created: false,
            run_id: false,
            source: false
        },
        Status = "invalid"
    ;   graph_summary_dict(Graph, Summary),
        TripleCount = Summary.triples,
        graph_metadata_dict(Graph, Metadata),
        metadata_presence(Metadata, MetadataChecks, Warnings0),
        findall(Error, validation_error(Graph, TripleCount, MetadataChecks, Error), Errors),
        metadata_warnings(Graph, MetadataChecks, Warnings1),
        append(Warnings0, Warnings1, Warnings),
        (   agent_graph_category(Graph, Category)
        ->  true
        ;   Category = uncontrolled
        ),
        (   Errors == []
        ->  Status = "valid"
        ;   Status = "invalid"
        )
    ).

validation_error(Graph, _TripleCount, _Checks, "Graph is not in the agent-controlled namespace") :-
    \+ agent_controlled_graph(Graph).
validation_error(_Graph, TripleCount, _Checks, "Graph is empty") :-
    TripleCount =:= 0.
validation_error(_Graph, _TripleCount, Checks, "Graph metadata is missing dcterms:created") :-
    Checks.created == false.
validation_error(_Graph, _TripleCount, Checks, "Graph metadata is missing agent:runId") :-
    Checks.run_id == false.

metadata_warnings(_Graph, Checks, ["Graph metadata does not include dcterms:source"]) :-
    Checks.source == false,
    !.
metadata_warnings(_, _, []).

metadata_presence(Metadata,
                  _{
                      controlled_graph: Controlled,
                      created: Created,
                      run_id: RunId,
                      source: Source
                   },
                  []) :-
    Controlled = Metadata.controlled_graph,
    truthy_text(Metadata.created, Created),
    truthy_text(Metadata.run_id, RunId),
    truthy_text(Metadata.source, Source).

graph_request(Request, Graph) :-
    (   memberchk(method(post), Request),
        json_content(Request)
    ->  http_read_json_dict(Request, Dict),
        dict_atom(Dict, graph, Graph)
    ;   http_parameters(Request, [graph(Graph, [])])
    ).

read_load_payload(Request, Payload) :-
    (   json_content(Request)
    ->  http_read_json_dict(Request, Dict0),
        Payload = Dict0
    ;   throw(error(domain_error(content_type, Request), _))
    ).

json_content(Request) :-
    memberchk(content_type(ContentType), Request),
    sub_atom(ContentType, 0, _, _, 'application/json').

proposal_graph(Payload, Graph, Stage, RunId) :-
    (   get_dict(stage, Payload, Stage0)
    ->  atom_string(Stage, Stage0)
    ;   Stage = proposal
    ),
    (   get_dict(graph, Payload, Graph0)
    ->  atom_string(Graph, Graph0)
    ;   graph_from_payload(Payload, Stage, RunId, Graph)
    ),
    (   get_dict(run_id, Payload, RunId0)
    ->  atom_string(RunId, RunId0)
    ;   graph_run_id(Graph, RunId)
    ).

graph_from_payload(Payload, Stage, RunId, Graph) :-
    dict_atom(Payload, run_id, RunId),
    graph_date(Payload, Date),
    atomic_list_concat(['urn:agent:', Stage, ':', Date, ':', RunId], Graph).

graph_date(Payload, Date) :-
    (   get_dict(date, Payload, Date0)
    ->  atom_string(Date, Date0)
    ;   get_time(Now),
        format_time(atom(Date), '%F', Now)
    ).

graph_run_id(Graph, RunId) :-
    atomic_list_concat(Parts, :, Graph),
    last(Parts, RunId).

ensure_allowed_stage(Stage) :-
    memberchk(Stage, [proposal, staging]),
    !.
ensure_allowed_stage(Stage) :-
    throw(error(domain_error(agent_stage, Stage), _)).

ensure_graph_is_new(Graph) :-
    (   rdf_graph(Graph)
    ->  (   nonempty_graph(Graph)
        ->  throw(error(permission_error(create, graph, Graph), _))
        ;   rdf_unload_graph(Graph)
        )
    ;   true
    ).

must_be_existing_graph(Graph) :-
    (   rdf_graph(Graph)
    ->  true
    ;   throw(error(existence_error(graph, Graph), _))
    ).

must_be_controlled_graph(Graph) :-
    must_be_existing_graph(Graph),
    (   agent_controlled_graph(Graph)
    ->  true
    ;   throw(error(permission_error(modify, graph, Graph), _))
    ).

forbid_accepted_graph(Graph) :-
    (   agent_graph_category(Graph, accepted)
    ->  throw(error(permission_error(delete, accepted_graph, Graph), _))
    ;   true
    ).

assert_graph_metadata(Graph, Stage, RunId, Payload) :-
    get_time(Now),
    format_time(atom(Timestamp), '%FT%TZ', Now),
    stage_type(Stage, Type),
    rdf_assert(Graph, rdf:type, Type, Graph),
    rdf_assert(Graph, agent:runId, literal(RunId), Graph),
    rdf_assert(Graph, agent:stage, literal(Stage), Graph),
    rdf_assert(Graph, dcterms:created, literal(type(xsd:dateTime, Timestamp)), Graph),
    assert_optional_metadata(Graph, dcterms:source, Payload, source),
    assert_optional_metadata(Graph, agent:promptVersion, Payload, prompt_version),
    assert_optional_metadata(Graph, agent:modelName, Payload, model),
    assert_optional_metadata(Graph, agent:confidence, Payload, confidence),
    assert_optional_metadata(Graph, agent:reviewNotes, Payload, review_notes).

assert_optional_metadata(Graph, Predicate, Payload, Key) :-
    (   get_dict(Key, Payload, Value0)
    ->  atom_string(Value, Value0),
        rdf_assert(Graph, Predicate, literal(Value), Graph)
    ;   true
    ).

stage_type(proposal, agent:'ProposalGraph').
stage_type(staging,  agent:'StagingGraph').
stage_type(accepted, agent:'AcceptedGraph').

agent_graph_category(Graph, proposal) :-
    atom(Graph),
    sub_atom(Graph, 0, _, _, 'urn:agent:proposal:').
agent_graph_category(Graph, staging) :-
    atom(Graph),
    sub_atom(Graph, 0, _, _, 'urn:agent:staging:').
agent_graph_category(Graph, accepted) :-
    atom(Graph),
    sub_atom(Graph, 0, _, _, 'urn:agent:accepted:').

agent_controlled_graph(Graph) :-
    agent_graph_category(Graph, _).

graph_summary_dict(Graph,
                   _{ graph: Graph,
                      category: Category,
                      triples: TripleCount,
                      persistent: Persistent
                    }) :-
    must_be_existing_graph(Graph),
    (   agent_graph_category(Graph, Category)
    ->  true
    ;   Category = uncontrolled
    ),
    graph_triple_count(Graph, TripleCount),
    (   rdf_graph_property(Graph, persistent(true))
    ->  Persistent = true
    ;   Persistent = false
    ).

graph_metadata_dict(Graph,
                    _{ controlled_graph: Controlled,
                       created: Created,
                       run_id: RunId,
                       source: Source,
                       prompt_version: PromptVersion,
                       model: Model,
                       confidence: Confidence,
                       review_notes: ReviewNotes
                     }) :-
    (   agent_controlled_graph(Graph)
    ->  Controlled = true
    ;   Controlled = false
    ),
    metadata_literal(Graph, dcterms:created, Created),
    metadata_literal(Graph, agent:runId, RunId),
    metadata_literal(Graph, dcterms:source, Source),
    metadata_literal(Graph, agent:promptVersion, PromptVersion),
    metadata_literal(Graph, agent:modelName, Model),
    metadata_literal(Graph, agent:confidence, Confidence),
    metadata_literal(Graph, agent:reviewNotes, ReviewNotes).

metadata_literal(Graph, Predicate, Value) :-
    rdf(Graph, Predicate, literal(Literal), Graph),
    !,
    literal_text(Literal, Value).
metadata_literal(_, _, "").

resource_json(Resource, _{id: Resource, display: Display}) :-
    (   rdf_global_id(NS:Local, Resource)
    ->  atomic_list_concat([NS, :, Local], Display)
    ;   Display = Resource
    ).

object_json(literal(type(Type, Value)),
            _{type: "typed-literal", datatype: Datatype, value: Value}) :-
    !,
    resource_display(Type, Datatype).
object_json(literal(lang(Lang, Value)),
            _{type: "lang-literal", lang: Lang, value: Value}) :-
    !.
object_json(literal(Value),
            _{type: "literal", value: Value}) :-
    !.
object_json(Object, _{type: "term", value: Text}) :-
    compound(Object),
    !,
    term_string(Object, Text).
object_json(Resource, Json) :-
    resource_json(Resource, Json).

literal_text(type(_, Value), Value) :- !.
literal_text(lang(_, Value), Value) :- !.
literal_text(Value, Value).

resource_display(Resource, Display) :-
    (   rdf_global_id(NS:Local, Resource)
    ->  atomic_list_concat([NS, :, Local], Display)
    ;   Display = Resource
    ).

dict_atom(Dict, Key, Atom) :-
    get_dict(Key, Dict, Value0),
    atom_string(Atom, Value0).

must_be_nonempty_text(Text, _Name) :-
    atom_string(Atom, Text),
    atom_length(Atom, Length),
    Length > 0,
    !.
must_be_nonempty_text(_, Name) :-
    throw(error(domain_error(nonempty_text, Name), _)).

truthy_text("", false) :- !.
truthy_text(_, true).

graph_triple_count(Graph, TripleCount) :-
    (   rdf_graph_property(Graph, triples(TripleCount))
    ->  true
    ;   TripleCount = 0
    ).

nonempty_graph(Graph) :-
    graph_triple_count(Graph, TripleCount),
    TripleCount > 0.

nonempty_agent_graph(Category) :-
    rdf_graph(Graph),
    agent_graph_category(Graph, Category),
    nonempty_graph(Graph).

first_n(N, List, Prefix) :-
    length(Prefix, N),
    append(Prefix, _, List),
    !.
first_n(_, List, List).

api_reply(Goal) :-
    catch(Goal, Error, reply_error(Error)).

reply_error(Error) :-
    exception_status(Error, Status),
    message_to_string(Error, Message),
    reply_json_dict(_{
        status: "error",
        error: Message
    }, [status(Status)]).

exception_status(error(existence_error(_, _), _), 404).
exception_status(error(permission_error(_, _, _), _), 403).
exception_status(error(domain_error(_, _), _), 400).
exception_status(_, 500).
