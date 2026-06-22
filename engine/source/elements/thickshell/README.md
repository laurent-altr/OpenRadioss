# Thick Shell Force Computation (`engine/source/elements/thickshell/`)

Computes internal forces for thick shell (continuum shell) elements — shell elements with solid topology but shell kinematic assumptions.

## Subdirectories

| Directory | Element | Notes |
|-----------|---------|-------|
| `solidec/` | Thick shell (8-node hex topology, shell kinematics) | Primary formulation |
| `solide8c/` | 8-node thick shell | Variant |
| `solide6c/` | 6-node thick shell (wedge topology) | Triangular thick shell |
| `solide16/` | 16-node thick shell | Quadratic through-thickness |

## Thick Shell vs Shell

| Feature | Shell (TYPE1) | Thick Shell (TYPE20) |
|---------|--------------|---------------------|
| Topology | Surface (2D) | Volume (3D) |
| Thickness integration | Through-thickness point loop | Solid Gauss points |
| Transverse shear | Kirchhoff/Mindlin assumption | Full 3D |
| Thickness change | Assumed constant or linear | Computed from 3D strain |
| Best for | Thin structures (t/L < 0.1) | Thick laminates, rubber |

Thick shells have separate nodes on top and bottom surfaces, allowing the thickness to change and the through-thickness normal stress to be non-zero — critical for rubber and foam materials.

## Related Documentation

- `engine/source/elements/README.md` — parent elements overview
- `engine/source/elements/shell/README.md` — thin shell formulations
