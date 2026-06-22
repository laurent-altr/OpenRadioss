# Shell Element Force Computation (`engine/source/elements/shell/`)

Computes internal forces for 4-node quadrilateral shell elements each time step. Three shell formulations are provided:

## Subdirectories

| Directory | Formulation | Element |
|-----------|-------------|---------|
| `coque/` | QEPH (Q4γ24 Efficiency Physical Hourglass stabilisation) | TYPE1 default |
| `coqueba/` | Belytschko-Tsay + physical stabilisation | TYPE9 |
| `coquez/` | Shell with Z-direction (thickness) update | TYPE11 |

## Key Files (top-level)

| File | Role |
|------|------|
| `coqini.F` | Shell group initialisation at start of engine run |
| `fequilibre.F` | Shell equilibrium check |
| `err_thk.F` | Thickness error detection |
| `shell_offset_wm_ini.F90` | Initialise shell offset for weighted mass approach |

## QEPH Formulation (`coque/`)

QEPH (Q4 with Physical Hourglass stabilisation) is the default and most-used shell:
- Single integration point in-plane (1-point reduced integration)
- Physical hourglass stabilisation using assumed strain
- No shear locking in thin-shell limit
- 5 or more integration points through thickness (Gauss-Lobatto)

The QEPH element correctly handles combined membrane + bending + transverse shear with a single in-plane integration point, making it O(4) cheaper per element than full 2×2 integration while maintaining accuracy.

## Related Documentation

- `starter/source/elements/README.md` — shell initialisation
- `engine/source/elements/README.md` — parent elements overview
- `engine/source/elements/sh3n/README.md` — triangular shell (3-node)
