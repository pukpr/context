# ClioPatria Server Evaluation - Quick Start

## Status: ✅ WORKING

The ClioPatria server has been successfully evaluated and debugged (January 2026). After several years of inactivity, the server **starts successfully** and is operational with SWI-Prolog 9.0.4.

## Quick Start

```bash
cd dynamic_context_server
./run-cloud.sh start
```

Access the server at: **http://localhost:3020/**

On a first-time setup with an empty `RDF-store`, open the web UI and run
**Repository -> Load Context Data** once. After that initial import, later
restarts will use the persisted store.

## Server Management

```bash
./run-cloud.sh start    # Start the server
./run-cloud.sh stop     # Stop the server
./run-cloud.sh restart  # Restart the server
./run-cloud.sh status   # Check server status
```

## Documentation

- **`EVALUATION_REPORT.md`** - Detailed technical evaluation of all issues found
- **`dynamic_context_server/DEPLOYMENT.md`** - Complete deployment and operations guide

## What Works

✅ Core ClioPatria server  
✅ HTTP server on port 3020  
✅ RDF triple store  
✅ Web interface  
✅ SPARQL queries  
✅ Configuration system  

## First-Time Data Load

If pages such as `/context_model/navigate?characteristics=windSpeed` return
empty results on a new installation, the server is up but the persistent RDF
store has not been seeded yet. Use **Repository -> Load Context Data** in the
browser to populate it.

## What Needs Optional Setup

⚠️ **Graphviz** (for datacloud visualization feature)

To enable datacloud visualization:
```bash
sudo apt-get install graphviz
```

Without graphviz, the datacloud visualization at `/datacloud` won't work, but all other features remain functional.

⚠️ **R Statistical Integration** (for full climate modeling features)

To enable R features, install:
```bash
sudo apt-get install r-base r-base-dev
swipl -g "pack_install(real, [interactive(false)])" -t halt
```

Without R, the server works for basic RDF/Semantic Web operations but advanced statistical modeling features will not be available.

## Previous Deployment Script

The old `nohup-run-cloud` script has been **replaced** by `run-cloud.sh` which includes:
- Proper process management
- PID tracking
- Log management
- Status checking
- Graceful shutdown

## System Requirements

- **OS**: Linux (tested on Ubuntu 24.04)
- **SWI-Prolog**: 9.0.4 or later
- **Port**: 3020 (configurable)
- **Optional**: Graphviz 2.43.0+ for datacloud visualization
- **Optional**: R 3.0+ for statistical features

## Support

See the detailed evaluation report (`EVALUATION_REPORT.md`) for:
- Complete list of all errors and warnings with severity levels
- Compatibility analysis
- Missing dependencies
- Recommended fixes
- Testing procedures
- Production deployment strategies
