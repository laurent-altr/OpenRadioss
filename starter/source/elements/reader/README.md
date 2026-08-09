# HyperMesh Element Readers (`starter/source/elements/reader/`)

HyperMesh (HM) binary format readers for all element types. These routines are called when the starter is invoked with a `.hm` input file instead of a text keyword deck.

## Key Files

| File | Role |
|------|------|
| `hm_read_node.F` | Read node coordinates from HM binary |
| `hm_preread_node.F` | Pre-pass: count nodes and determine ID ranges |
| `hm_read_solid.F` | Read solid element connectivity |
| `hm_read_shell.F` | Read 4-node shell elements |
| `hm_read_sh3n.F` | Read 3-node shell elements |
| `hm_read_beam.F` | Read beam elements |
| `hm_read_quad.F` | Read quadrilateral shell elements (generic) |
| `hm_read_tria.F` | Read triangular elements |
| `hm_read_spring.F` | Read spring/damper elements |
| `hm_read_truss.F` | Read truss elements |
| `hm_read_sphcel.F` | Read SPH particle cells |
| `hm_read_xelem.F` | Read X-element (extra element) data |
| `hm_preread_xelem.F` | Pre-pass count for X-elements |
| `hm_read_rivet.F` | Read rivet connector elements |
| `hm_read_merge_node.F` | Read HM node merge definitions |
| `rivet0.F` | Rivet element setup from HM data |

## HM Binary Format

The HyperMesh binary (`.hm`) format stores model data in tagged blocks. Each `hm_read_*.F` file:
1. Locates the relevant block by tag ID
2. Reads the binary record into a Fortran array
3. Converts HM internal IDs to OpenRadioss IDs
4. Populates the same data structures as the text keyword path

After this reader pass, the model is assembled identically to the text-input path — the rest of the starter pipeline is format-agnostic.

## Related Documentation

- `starter/source/devtools/README.md` — HM reader subsystem overview
- `starter/source/elements/README.md` — parent elements directory
