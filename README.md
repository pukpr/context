# Context Project

## Overview
_Context_ is a modeling toolkit built upon ClioPatria, providing context modeling, RDF triple store, and web interface features.

## Quick Start
For installation and rapid startup instructions, see [QUICKSTART.md](QUICKSTART.md).
- **ClioPatria Server:** SWI-Prolog-based server, operational on port 3020.
- Core features: HTTP server, RDF triple store, SPARQL queries, configuration.

## Deployment Guide
Detailed deployment, prerequisites, and system configuration are covered in [dynamic_context_server/DEPLOYMENT.md](dynamic_context_server/DEPLOYMENT.md).
- Basic and full deployment instructions (including statistical features via R, recently deprecated in favor of native Prolog and JavaScript graphs).
- Server management, logs, port and public access configuration.
- Production deployment advice (systemd service guide).

## Evaluation and Debugging Report
See [EVALUATION_REPORT.md](EVALUATION_REPORT.md) for a full technical analysis:
- Startup tests, identified errors, and their severity.
- Compatibility issues and recommended fixes.

## ClioPatria Toolkit
More about the underlying toolkit can be found in [ClioPatria/README.md](ClioPatria/README.md).
- Semantic Web features, SPARQL server, reasoning libraries, admin interface, user management.

## Datacloud Visualization
For visualization of RDF data relationships:
- [dynamic_context_server/cpack/cloud/README.md](dynamic_context_server/cpack/cloud/README.md) explains the datacloud feature, requirements (Graphviz), and usage for interactive SVG visualizations.

## Documentation Map
- [QUICKSTART.md](QUICKSTART.md): rapid startup, tested configurations.
- [EVALUATION_REPORT.md](EVALUATION_REPORT.md): technical status, issues, resolutions.
- [dynamic_context_server/DEPLOYMENT.md](dynamic_context_server/DEPLOYMENT.md): production, ops, and advanced setup.
- [ClioPatria/README.md](ClioPatria/README.md): toolkit internals and user interface.
- [dynamic_context_server/cpack/cloud/README.md](dynamic_context_server/cpack/cloud/README.md): datacloud visual features.

## Final Report
The [`Final_report`](./Final_report) directory contains the key documentation summarizing the project's outcomes and supporting details. You will find:

- **Main Final Report:** A comprehensive review of all major findings, methodologies, and conclusions.
- **Broken-out PDFs:** Supplementary documents or appendices separated for convenient access, covering specific sections or detailed datasets highlighted in the main report.

Explore the directory [here](./Final_report) to access the full set of reports.

## Links
- [ClioPatria project](https://www.swi-prolog.org)
- [SWI-Prolog](https://www.swi-prolog.org)

---
_For full content and latest updates, view each file directly in the GitHub UI:_  
- [Markdown file search results for pukpr/context](https://github.com/pukpr/context/search?q=path%3A%2F.*%5C.md%24)

---

_Last updated: 2026-02-26_
