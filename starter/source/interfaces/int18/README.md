# TYPE18 Interface — LAW151 Cohesive (`starter/source/interfaces/int18/`)

Starter reader for /INTER/TYPE18: cohesive zone interface using LAW151 cohesive material model.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type18.F` | Parse /INTER/TYPE18 card parameters |
| `int18_law151_alloc.F` | Allocate LAW151 cohesive material data arrays |
| `int18_law151_init.F` | Initialise LAW151 traction-separation law parameters |

## Description

TYPE18 uses cohesive zone elements (zero-thickness interface elements) with the LAW151 traction-separation law. The law defines normal and tangential stiffness, peak traction, and fracture energy for Mode I, II, and mixed-mode delamination. Used for composite delamination and adhesive joint simulation.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/int09/README.md` — TYPE9 tied-with-failure contact
