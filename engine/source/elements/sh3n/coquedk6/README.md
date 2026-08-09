# DKT6 Shell (`engine/source/elements/sh3n/coquedk6/`)

Discrete Kirchhoff Triangle with 6 nodes (midside nodes added for quadratic geometry representation).

## Key Files

| File | Role |
|------|------|
| `cdk6coor3.F` | 6-node triangle coordinates (quadratic geometry) |
| `cdk6defo3.F` | DKT6 strain computation |
| `cdk6deri3.F` | Shape function derivatives (6-node) |
| `cdk6fcum3.F` | Force accumulation to 6 nodes |
| `cdk6bc3.F` | Boundary condition handling for midside nodes |

## Formulation

DKT6 extends the DKT triangle with midside nodes, enabling quadratic geometry mapping. This improves accuracy for curved shells and allows connecting to second-order solid elements (tetrahedra with midside nodes) without introducing kinematic incompatibilities. Used in mixed-element meshes where geometric accuracy of the curved mid-surface is important.

## Related Documentation

- `engine/source/elements/sh3n/README.md` — parent sh3n directory
- `engine/source/elements/sh3n/coquedk/README.md` — linear DKT (3-node)
