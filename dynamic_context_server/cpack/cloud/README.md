# Datacloud Visualization

This package provides datacloud visualization functionality for ClioPatria, allowing you to visualize the relationships between different data sources in your RDF triple store as a graph.

## Overview

The datacloud feature generates visual representations of CloudNodes - collections of RDF graphs that are logically grouped together. It shows:
- The size of each CloudNode (number of triples)
- Links between CloudNodes (based on RDF relationships)
- Interactive visualization using Graphviz

## Requirements

### Required
- **SWI-Prolog 9.0.4+**
- **Graphviz 2.42.0+** with the following tools:
  - `dot` - Hierarchical graph layout
  - `fdp` - Force-directed placement (default)
  - `neato` - Spring model layout

### Installation

On Ubuntu/Debian:
```bash
sudo apt-get install graphviz
```

Verify installation:
```bash
dot -V
fdp -V
neato -V
```

## Usage

### Web Interface

Once the server is running, access the datacloud visualization at:

```
http://localhost:3020/datacloud
```

This endpoint:
1. Generates a DOT graph representation of your CloudNodes
2. Renders it using Graphviz (fdp by default)
3. Serves an interactive SVG visualization

### Programmatic Usage

You can also use the datacloud predicates programmatically:

```prolog
:- use_module(library(datacloud)).

% Generate a DOT graph to an output stream
?- open('datacloud.dot', write, Out),
   write_cloud_graph(Out, []),
   close(Out).

% Query links between CloudNodes
?- datacloud_link(SourceSet, TargetSet, Triple, []).
```

## Configuration

The datacloud feature can be configured via settings:

```prolog
% In your config file
:- use_module(library(settings)).

% Set the output format (svg, png, pdf, etc.)
:- set_setting(cloud:format, svg).

% Set the layout engine (dot, fdp, neato)
:- set_setting(cloud:renderer, fdp).

% Set the output file location
:- set_setting(cloud:image_file, 'datacloud.svg').
```

### Available Renderers

- **fdp** (default) - Force-directed placement, good for general graphs
- **dot** - Hierarchical layout, good for directed acyclic graphs
- **neato** - Spring model, good for smaller symmetric graphs

## How It Works

### CloudNodes

A CloudNode is defined in RDF manifest files with:
- Type: `lib:CloudNode`
- Title: A descriptive name (using `dcterms:title`)
- Graphs: One or more RDF graphs loaded from this source

### Link Detection

The datacloud algorithm detects links between CloudNodes by finding:
1. Triples where subject and object are in different graphs
2. Both subject and object have `rdf:type` statements
3. Neither subject nor object are blank nodes
4. The graphs belong to different CloudNodes

### Visualization

The generated visualization shows:
- **Nodes**: Each CloudNode, sized by number of triples
- **Edges**: Links between CloudNodes, weighted by number of connections
- **Tooltips**: Hover information showing:
  - CloudNode title and triple count
  - Number of links between nodes

## Testing

A test script is provided to verify the datacloud functionality:

```bash
cd dynamic_context_server
swipl -s test_datacloud.pl
```

This tests:
- Datacloud module loading
- Graphviz tool availability (dot, fdp, neato)
- Graph generation capability

## Troubleshooting

### "Graphviz not found" error

**Problem**: The server can't find dot/fdp/neato commands.

**Solution**: 
```bash
sudo apt-get install graphviz
which dot  # Should show /usr/bin/dot
```

### Empty datacloud visualization

**Problem**: The datacloud appears empty or shows "No CloudNodes found".

**Cause**: No RDF data with CloudNode entries has been loaded.

**Solution**: Ensure your RDF manifest files include CloudNode entries:
```turtle
@prefix lib: <http://cliopatria.swi-prolog.org/library/> .
@prefix dcterms: <http://purl.org/dc/terms/> .

<http://example.org/mydata>
    a lib:CloudNode ;
    dcterms:title "My Data Source"@en .
```

### Renderer timeout

**Problem**: Datacloud generation times out.

**Cause**: Very large datasets or complex graphs.

**Solution**: 
1. Reduce the dataset size
2. Try a different renderer (e.g., switch from fdp to neato)
3. Increase the HTTP timeout in settings

## API Reference

### write_cloud_graph(+Out, +Options)

Generates a DOT graph representation of CloudNodes.

- **Out**: Output stream to write the DOT graph
- **Options**: List of options:
  - `min_size(Float)`: Minimum node size (default: 0.5)
  - `max_size(Float)`: Maximum node size (default: 3)
  - `unload_manifests(Boolean)`: Unload manifest RDF after processing (default: false)

### datacloud_link(?SourceSet, ?TargetSet, -Triple, +Options)

Query links between CloudNodes.

- **SourceSet**: Source CloudNode identifier
- **TargetSet**: Target CloudNode identifier
- **Triple**: The RDF triple connecting them (rdf(S,P,O))
- **Options**: Same as write_cloud_graph/2

## Performance Considerations

For large datasets:
- The datacloud generation is cached (regenerated only if missing)
- First access may take several seconds
- Subsequent accesses are instant (serves cached file)
- Clear cache by deleting the image file specified in settings

## Examples

### Basic Usage

```prolog
% Load and visualize datacloud
?- cp_server.
% Visit http://localhost:3020/datacloud in browser
```

### Custom Visualization

```prolog
% Generate with custom settings
?- open('my_cloud.dot', write, Out),
   write_cloud_graph(Out, [
       min_size(1.0),
       max_size(5.0),
       unload_manifests(true)
   ]),
   close(Out).

% Render with neato instead of fdp
?- set_setting(cloud:renderer, neato).
```

## See Also

- [ClioPatria Documentation](http://cliopatria.swi-prolog.org/)
- [Graphviz Documentation](https://graphviz.org/documentation/)
- [RDF Library System](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/semweb.html%27))

## License

This is part of the ClioPatria SeRQL and SPARQL server, licensed under GPL v2+.

## Credits

- Author: Victor de Boer
- Institution: VU University Amsterdam
