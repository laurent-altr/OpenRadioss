# Starter Model Subsystem

This subsystem manages the global model assembly — collecting all the parsed data (nodes, elements, materials, groups, interfaces) into a coherent data structure, then performing model-level operations before the restart file is written.

## Directory Structure

```
model/
├── assembling/    — Global model assembly (connectivity, groups, sets)
├── box/           — Bounding box computation
├── group/         — Group management and cross-referencing
├── mesh/          — Mesh-level operations (renumbering, quality metrics)
└── remesh/        — Adaptive mesh setup (initial AMR configuration)
```

## Key Operations

### Assembly (`assembling/`)

After all keywords are read, the model assembler:
1. Builds global connectivity arrays (node → element lists)
2. Cross-references groups to elements and nodes
3. Resolves part-based references to element lists
4. Computes node-element neighbourhoods (used by non-local models and AMR)

### Mesh Quality (`mesh/`)

Computes element quality metrics reported in the `.out` file:
- Aspect ratio (for shells and solids)
- Jacobian / distortion index
- Warpage (for shells)
- Minimum edge length (critical for DT estimate)

Elements outside quality thresholds trigger warnings.

### Bounding Box (`box/`)

Computes the model bounding box used for:
- ALE domain initialisation
- Contact search zone definition
- Output reference dimensions

### Group Management (`group/`)

Resolves group membership — a `/GRPART` group by property ID, for example, is expanded here into a list of element IDs. All group types are normalised to explicit member lists by this step.

### Adaptive Mesh Setup (`remesh/`)

If AMR is requested, sets up the refinement data structures at t=0:
- Marks initially refined elements
- Builds the refinement tree hierarchy
- Computes initial error estimates

## Related Documentation

- `engine/source/model/README.md` — runtime model data (engine side)
- `starter/source/elements/README.md` — element parsing that feeds model assembly
- `starter/source/spmd/README.md` — domain decomposition that operates on the assembled model
