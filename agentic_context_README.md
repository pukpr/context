# Agentic Context

Agentic Context is a wrapper project for adding LLM-assisted ontology, model-library, and knowledgebase growth around the legacy Dynamic Context Server contained in [`pukpr/context`](https://github.com/pukpr/context). The intent is not to rewrite the original Prolog/ClioPatria system, but to preserve it as the semantic runtime while adding an isolated agent layer for proposal generation, validation, provenance, and reviewed promotion.

The legacy repository already contains the core DCS architecture: vendored ClioPatria, the `dynamic_context_server/` application, RDF/Turtle manifests, SWEET-aligned terminology, Prolog modules, domain models, and a persistent RDF store mechanism through ClioPatria’s `rdf_db` stack. Agentic Context adds a sidecar LLM runtime and a small ClioPatria overlay so that agent-generated knowledge can be staged safely in named graphs before anything is accepted.

## Goals

- Preserve the existing Dynamic Context Server and knowledgebase in [`pukpr/context`](https://github.com/pukpr/context).
- Add a reversible overlay instead of modifying legacy modules directly.
- Let LLM agents ingest documents, model notes, code, datasets, and standards, then propose structured RDF and model artifacts.
- Keep agent output untrusted until it passes syntax, schema, provenance, unit, and regression checks.
- Promote accepted knowledge through GitHub pull requests rather than direct mutation of the legacy repo.

## Non-goals

- Do not rewrite ClioPatria during the MVP.
- Do not merge LLM-generated triples directly into the accepted knowledgebase.
- Do not modify `100-used_modules.pl`, `Manifest.ttl`, existing `context_*.pl` modules, `settings.db`, or `users.db` during the MVP.
- Do not put LLM API calls inside Prolog initially.

## Proposed repository layout

```text
agentic-context/
├── README.md
├── docker-compose.yml
├── Dockerfile.cliopatria
├── Dockerfile.agent
├── upstream/
│   └── context/                     # git submodule pinned to github.com/pukpr/context
├── overlay/
│   └── dynamic_context_server/
│       ├── config-enabled/
│       │   └── agent.pl             # one-line loader for the agent cpack
│       └── cpack/
│           └── agent/
│               ├── config-available/
│               │   └── agent.pl
│               ├── lib/
│               │   ├── agent_api.pl
│               │   └── agent_provenance.pl
│               └── rdf/
│                   └── agent.ttl
├── sidecar/
│   ├── agent/                       # LLM orchestration
│   ├── prompts/                     # extraction, alignment, critique, validation prompts
│   ├── tools/                       # SPARQL, RDF upload, graph diff, model eval tools
│   ├── eval/                        # golden tasks and frozen-KB evaluations
│   └── tests/
├── ontology/
│   ├── proposals/                   # raw agent-authored TTL by run ID
│   ├── staging/                     # validation-passing but not accepted
│   └── accepted/                    # reviewed and promoted TTL
├── models/
│   ├── proposals/                   # proposed Prolog/Python/Modelica/model metadata
│   ├── staging/
│   └── accepted/
├── scripts/
│   ├── bootstrap.sh
│   ├── start.sh
│   ├── stop.sh
│   ├── promote.sh
│   └── snapshot_kb.sh
└── .github/
    └── workflows/
        ├── ci.yml
        └── eval.yml
```

## Runtime architecture

```text
┌─────────────────────┐
│   User / Browser    │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────────────────────────────┐
│ Legacy DCS / ClioPatria / SWI-Prolog         │
│                                             │
│ - dynamic_context_server/run.pl             │
│ - SPARQL endpoint                           │
│ - RDF/OWL/Turtle knowledgebase              │
│ - Prolog reasoning and model handlers       │
│ - agent cpack overlay                       │
└──────────┬──────────────────────┬───────────┘
           │                      │
           │ HTTP / SPARQL        │ Named graphs
           ▼                      ▼
┌─────────────────────┐   ┌─────────────────────┐
│ LLM sidecar service │   │ RDF proposal graphs │
│                     │   │                     │
│ - ingest            │   │ <agent/proposal/...>│
│ - extract triples   │   │ <agent/staging>     │
│ - align ontology    │   │ <agent/accepted/...>│
│ - validate          │   └─────────────────────┘
│ - open PRs          │
└─────────────────────┘
```

## Legacy system integration points

The wrapper should use existing ClioPatria and DCS mechanisms where possible:

- `dynamic_context_server/run.pl` remains the canonical application entry point.
- `config-enabled/agent.pl` loads only the new agent cpack.
- `cpack/agent/` follows the existing ClioPatria package pattern already used by `cpack/cloud/`.
- The sidecar talks to the DCS over HTTP, SPARQL, and RDF upload APIs rather than linking directly into Prolog.
- Agent assertions are loaded into named graphs so they can be queried, inspected, unloaded, or promoted independently.

## Agent cpack API sketch

The first Prolog overlay should stay small and boring:

```text
GET  /agent/status
POST /agent/load_proposal
POST /agent/unload_proposal
POST /agent/validate_graph
GET  /agent/diff
GET  /agent/provenance
```

The Prolog side should handle graph loading, unloading, provenance inspection, and lightweight validation. The sidecar should handle LLM calls, vector search, document parsing, prompt orchestration, and GitHub integration.

## Knowledge lifecycle

### Proposal

An agent run writes output into:

```text
ontology/proposals/<run-id>/proposal.ttl
models/proposals/<run-id>/
```

Every proposal includes:

- source document or input artifact
- extraction prompt version
- model used
- timestamp
- proposed triples or model files
- provenance links
- confidence and review notes

### Staging

The proposal is loaded into a named graph:

```text
<agent/proposal/<date>/<run-id>>
```

The staging graph is queryable through the DCS but is not treated as accepted knowledge.

### Validation

Validation should include:

- RDF/Turtle syntax checks
- namespace checks
- SHACL or OWL-RL schema checks
- duplicate/entity alignment checks
- unit and dimensional checks
- Prolog plunit checks where applicable
- model-specific numerical smoke tests
- golden-query regression tests

### Promotion

Validated proposals move from `proposals/` to `staging/` and then to `accepted/`. Promotion opens a GitHub PR against `agentic-context`, not directly against the legacy `pukpr/context` repo.

## Suggested domain MVP

The first domain should be narrow and scientifically useful. A good candidate is a geophysical harmonic-model library:

- phenomena: QBO, ENSO, Chandler wobble, tides, sea-level height
- forcing terms: draconic, tropical, anomalistic, nodal, annual, semiannual
- datasets: source, cadence, span, preprocessing, gaps
- model forms: harmonic regression, nonlinear regression, symbolic regression
- validation artifacts: residuals, phase constraints, cross-validation windows
- outputs: equations, generated plots, notebook links, HTML viewers

This MVP would demonstrate that the DCS can grow as a semantically searchable scientific model library rather than as a generic document chatbot.

## First milestone

The first milestone should prove the full loop without changing the legacy codebase:

1. Pin `pukpr/context` as a submodule.
2. Build a Docker image that boots `dynamic_context_server/run.pl`.
3. Mount an overlay containing `cpack/agent/`.
4. Expose `/agent/status`.
5. Load one small Turtle proposal into a named graph.
6. Query the proposal through SPARQL.
7. Validate the proposal.
8. Generate a graph diff.
9. Promote the proposal through a PR.

## Operational cautions

- The legacy repo includes old vendored ClioPatria code and should be pinned to a known-good SWI-Prolog runtime.
- Runtime state such as `settings.db`, `users.db`, and `RDF-store/` should be mounted as volumes, not treated as source artifacts.
- R integration should either be fixed in Docker or disabled for early MVP work to reduce noisy startup errors.
- Agent modules should use an `agent_*` namespace and qualified Prolog calls to avoid global predicate conflicts.
- Bulk RDF loads should be serialized to avoid persistent-store locking and journal issues.

## Definition of done for MVP

The MVP is complete when a fresh clone can:

1. Start the legacy DCS in Docker.
2. Load the base knowledgebase.
3. Start the agent sidecar.
4. Submit one generated ontology proposal.
5. Load it into an isolated named graph.
6. Validate it.
7. Query it.
8. Produce a human-readable diff.
9. Open or prepare a reviewable promotion PR.

