# SFEM Tet4 Solid (`engine/source/elements/solid/solide4_sfem/`)

Smoothed Finite Element Method (SFEM) variant of the 4-node tetrahedral element for improved accuracy without mesh sensitivity.

## Key Files

| File | Role |
|------|------|
| `s10_icp.F` | Integration point coordinates (shared with tet10) |
| `s10sigp3.F` | Stress at integration points |
| `s10volj.F` | Volume/Jacobian for SFEM tet |
| `s10volnod3.F` / `s10volnodt3.F` | Volume per node (standard / thermal) |

## Formulation

SFEM (Smoothed FEM, Liu et al. 2007) smooths the strain field over nodal domains rather than element domains, producing a model that lies between lower and upper energy bounds. For tet4 meshes this eliminates volumetric locking and gives results comparable to tet10 at tet4 cost. Used via `common_source/modules/elements/sfem_mod.F90`.

## Related Documentation

- `engine/source/elements/solid/README.md` — parent solid directory
- `engine/source/elements/solid/solide4/README.md` — standard tet4
- `common_source/modules/elements/README.md` — sfem_mod
