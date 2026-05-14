# Initial GitHub issue checklist for Agentic Context

This checklist assumes a new wrapper repository named `agentic-context` that pins [`pukpr/context`](https://github.com/pukpr/context) as an upstream submodule and adds an isolated agent overlay.

## Milestone: Repository bootstrap

### Issue: Create wrapper repository skeleton

Create the initial `agentic-context` repository structure.

Acceptance criteria:

- `README.md` exists.
- `upstream/`, `overlay/`, `sidecar/`, `ontology/`, `models/`, `scripts/`, and `.github/workflows/` directories exist.
- `ontology/{proposals,staging,accepted}/` directories exist.
- `models/{proposals,staging,accepted}/` directories exist.
- Empty directories contain `.gitkeep` files if needed.

### Issue: Add `pukpr/context` as a pinned submodule

Add the legacy DCS repository as a submodule under `upstream/context`.

Acceptance criteria:

- `upstream/context` points to `https://github.com/pukpr/context`.
- The submodule is pinned to a specific commit SHA.
- `scripts/bootstrap.sh` initializes and updates submodules.
- README documents how to update the submodule intentionally.

### Issue: Add Dockerized ClioPatria runtime

Create a Docker image that can boot the legacy DCS.

Acceptance criteria:

- `Dockerfile.cliopatria` installs SWI-Prolog, Graphviz, and other required runtime dependencies.
- `docker-compose.yml` exposes the DCS on port `3020`.
- `scripts/start.sh` starts the stack.
- `scripts/stop.sh` stops the stack.
- A fresh clone can reach the DCS landing page.

### Issue: Separate runtime state from source

Ensure mutable ClioPatria state is mounted as runtime data.

Acceptance criteria:

- `RDF-store/` is volume-mounted or ignored.
- `settings.db` and `users.db` are not modified in the upstream submodule during normal development.
- Runtime state paths are documented.
- `git status` remains clean after boot and shutdown.

## Milestone: Agent cpack overlay

### Issue: Add minimal `cpack/agent` overlay

Add a new ClioPatria cpack without modifying legacy DCS modules.

Acceptance criteria:

- `overlay/dynamic_context_server/config-enabled/agent.pl` loads the agent cpack.
- `overlay/dynamic_context_server/cpack/agent/config-available/agent.pl` exists.
- The overlay is copied or mounted into the runtime container.
- Removing the overlay fully disables agent functionality.

### Issue: Implement `/agent/status`

Expose a minimal health endpoint from the Prolog overlay.

Acceptance criteria:

- `GET /agent/status` returns JSON or simple text.
- Endpoint confirms Prolog server is live.
- Endpoint confirms agent cpack is loaded.
- Endpoint does not require LLM sidecar availability.

### Issue: Implement named-graph proposal loading

Add an endpoint for loading agent-generated Turtle into a named graph.

Acceptance criteria:

- `POST /agent/load_proposal` accepts a proposal file or payload.
- Proposal is loaded into a graph named like `<agent/proposal/<date>/<run-id>>`.
- Existing accepted graphs are not modified.
- Endpoint returns graph URI, triple count, and load status.

### Issue: Implement proposal unloading

Add rollback support for staged proposals.

Acceptance criteria:

- `POST /agent/unload_proposal` unloads a named proposal graph.
- Endpoint refuses to unload accepted/base graphs.
- Endpoint reports graph URI and number of removed triples.

### Issue: Implement graph diff endpoint

Provide a readable difference between a proposal graph and accepted knowledge.

Acceptance criteria:

- `GET /agent/diff?graph=<graph-uri>` returns added triples.
- Output includes subject, predicate, object, graph, and provenance if available.
- Output can be consumed by the sidecar and rendered in Markdown.

## Milestone: Sidecar agent service

### Issue: Add sidecar service skeleton

Create the Python or TypeScript agent runtime.

Acceptance criteria:

- `Dockerfile.agent` builds successfully.
- `sidecar/agent/` contains application entry point.
- `sidecar/tests/` contains at least one smoke test.
- `docker-compose.yml` starts the sidecar alongside DCS.

### Issue: Add SPARQL query tool

Allow the sidecar to query the DCS knowledgebase.

Acceptance criteria:

- Tool accepts a SPARQL query string.
- Tool sends query to DCS `/sparql/` endpoint.
- Tool returns structured JSON.
- Tool has tests using a known query.

### Issue: Add proposal writer

Create a sidecar tool that writes generated RDF proposals to disk.

Acceptance criteria:

- Tool creates `ontology/proposals/<run-id>/proposal.ttl`.
- Tool writes metadata including source, timestamp, prompt version, and model.
- Tool does not overwrite an existing run directory.

### Issue: Add document ingestion prototype

Implement one ingestion path for a short Markdown or PDF-derived text input.

Acceptance criteria:

- Input document is parsed into chunks.
- Agent extracts candidate entities, relations, units, and provenance.
- Output is serialized as Turtle.
- Extraction prompt is versioned under `sidecar/prompts/`.

### Issue: Add ontology alignment prototype

Map extracted concepts to existing terms where possible.

Acceptance criteria:

- Sidecar queries existing DCS terms before minting new ones.
- New local terms use a predictable namespace.
- Ambiguous mappings are flagged for human review.
- Output includes alignment notes.

## Milestone: Validation and promotion

### Issue: Add RDF syntax validation

Validate proposal Turtle before loading it into DCS.

Acceptance criteria:

- CI fails on invalid Turtle.
- Local validation script reports file, line, and error.
- Validation runs before `/agent/load_proposal`.

### Issue: Add schema validation

Add SHACL, OWL-RL, or equivalent structural checks.

Acceptance criteria:

- Basic required fields are enforced for agent-generated entities.
- Provenance fields are required.
- Invalid proposals fail before promotion.
- Validation report is written to the run directory.

### Issue: Add unit and dimensional checks

Add early checks for scientific/modeling quantities.

Acceptance criteria:

- Numeric quantities with units are parsed.
- Known unit dimensions are validated.
- Incompatible unit relationships are flagged.
- Results are included in validation report.

### Issue: Add Prolog smoke tests

Run a minimal set of Prolog tests in CI.

Acceptance criteria:

- CI boots SWI-Prolog with the DCS load path.
- CI runs available `plunit` tests where feasible.
- CI reports legacy warnings separately from blocking failures.
- Agent overlay tests are blocking.

### Issue: Add promotion script

Create `scripts/promote.sh` to move proposals through the lifecycle.

Acceptance criteria:

- Script promotes `proposals/<run-id>` to `staging`.
- Script promotes validated staging artifacts to `accepted`.
- Script refuses promotion if validation report is missing or failing.
- Script can produce a PR-ready branch.

## Milestone: Geophysical model-library pilot

### Issue: Define pilot ontology terms

Create a small extension vocabulary for geophysical harmonic modeling.

Acceptance criteria:

- Terms cover phenomenon, dataset, forcing component, model, equation, parameter, validation artifact, and visualization.
- Terms are aligned to existing DCS/SWEET terms where possible.
- Terms are documented in Turtle and Markdown.

### Issue: Ingest one geophysical model note

Use one controlled input document as the first agent-ingested example.

Acceptance criteria:

- Agent extracts at least one phenomenon, dataset, model, forcing term, and validation artifact.
- Proposal includes source provenance.
- Proposal loads into a named graph.
- Diff is human-readable.

### Issue: Add one generated model artifact

Allow the agent to propose a model artifact without loading it into legacy modules.

Acceptance criteria:

- Artifact is written under `models/proposals/<run-id>/`.
- Artifact metadata is represented in RDF.
- Artifact is not added to `100-used_modules.pl`.
- Artifact can be promoted into the wrapper overlay only.

### Issue: Add golden queries for the pilot

Create fixed queries that prove the model library is useful.

Acceptance criteria:

- Query: list models applicable to a phenomenon.
- Query: list forcing terms used by a model.
- Query: list datasets and preprocessing metadata.
- Query: list validation artifacts and generated visualizations.

## Milestone: CI and evaluation

### Issue: Add CI workflow

Run core checks on every PR.

Acceptance criteria:

- RDF syntax validation runs.
- Sidecar unit tests run.
- Prolog smoke boot runs.
- Agent overlay endpoint tests run.

### Issue: Add frozen-KB evaluation workflow

Create repeatable evaluations over a pinned knowledgebase snapshot.

Acceptance criteria:

- `scripts/snapshot_kb.sh` exports a frozen RDF snapshot.
- `eval.yml` runs golden SPARQL/query tasks.
- Evaluation results are stored as artifacts.
- Promotion is blocked if golden queries regress.

### Issue: Add provenance report generation

Generate a review report for every proposal.

Acceptance criteria:

- Report includes source artifacts.
- Report includes generated triples.
- Report includes validation results.
- Report includes open questions and low-confidence mappings.
- Report is Markdown and suitable for PR review.

