# Sub-Interface Output (`starter/source/output/subinterface/`)

Initialises sub-interface output — detailed time-history recording of individual contact pair forces within a contact interface.

## Key Files

| File | Role |
|------|------|
| `hm_read_intsub.F` | HM binary reader for sub-interface definitions |
| `inintsub_7.F` | Initialise sub-interface output for TYPE7 contact |
| `inintsub_11.F` | Initialise sub-interface output for TYPE11 contact |
| `inintsub_25.F` | Initialise sub-interface output for TYPE25 contact |
| `get_edge_fic_node.F90` | Get fictitious edge nodes for sub-interface boundary tracking |

## Sub-Interface Output

Standard contact output writes the total contact force on a surface. Sub-interface output records individual node-to-segment pair forces, enabling:
- Identifying the specific location of maximum contact force
- Tracking contact force distribution evolution
- Post-processing pressure maps over a contact surface

Sub-interface output is expensive (one TH record per contact pair) and is typically enabled only for a small region of interest.

## Related Documentation

- `starter/source/output/README.md` — parent output directory
- `starter/source/interfaces/README.md` — contact interface types
