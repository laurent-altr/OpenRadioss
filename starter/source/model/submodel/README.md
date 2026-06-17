# Submodel Subsystem

This subsystem implements `/SUBMODEL` — the ability to embed a finer local model within a coarser global model, with boundary conditions driven by the global model solution.

## Key Files

| File | Role |
|------|------|
| `lecsubmod.F` | Read `/SUBMODEL` keyword — submodel definition and reference to global model |
| `lectranssub.F` | Read transformation applied to the submodel (translation, rotation, scale) |
| `merge.F` | Merge submodel mesh into the global mesh at the boundary |
| `merge_cnod_cnod.F` | Merge coincident nodes at the submodel boundary |
| `3points_to_frame.F` | Compute local reference frame from three points (for submodel orientation) |
| `euler_mrot.F` | Euler rotation matrix computation |
| `euler_vrot.F` | Euler vector rotation |
| `rtranspos.F` | Transpose rotation matrix |

## What is a Submodel?

A submodel (`/SUBMODEL`) allows zooming into a region of interest with a refined mesh while keeping the global model at coarser resolution. The submodel is driven by:
- Interpolated boundary displacements (from the global model result)
- The submodel boundary nodes follow the global solution

This is used for fatigue and fracture analyses where local stress concentrations need fine resolution not practical in the full global model.

## Processing Steps

1. Read the submodel definition (refined mesh file reference, position/orientation)
2. Apply the transformation to position the submodel in global coordinates
3. Identify boundary nodes (nodes that lie on or near the global mesh)
4. Merge boundary nodes with the global mesh or set up interpolation mapping
5. Write the submodel mesh and mapping data to the restart file

## Related Documentation

- `starter/source/elements/nodes/README.md` — node merging utilities
- `starter/source/model/README.md` — parent model assembly
