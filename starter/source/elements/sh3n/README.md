# 3-Node Shell Initialisation (`starter/source/elements/sh3n/`)

Reads and initialises 3-node (triangular) shell elements. Three formulations are supported:

| Subdirectory | Formulation |
|---|---|
| `coque3n/` | Standard DKT (Discrete Kirchhoff Triangle) — default for `/SH3N` |
| `coquedk/` | Degenerate Kirchhoff — simplified variant |
| `coquedk6/` | Kirchhoff with 6 DOF per node (drilling rotation) |

## Key Files (top-level)

| File | Role |
|------|------|
| `get_sort_key_shell3n.F90` | Generates sort key for 3-node shells to order groups in restart file |

## What Each Formulation Provides

### `coque3n/` — DKT Triangle
Standard DKT formulation; the most commonly used 3-node shell in OpenRadioss. Uses the discrete Kirchhoff constraint to enforce zero transverse shear, giving good thin-shell accuracy without shear locking.

### `coquedk/` — Degenerate Kirchhoff
A simplified degenerate version. Less accurate but faster; used for coarse mesh regions.

### `coquedk6/` — 6-DOF Kirchhoff
Adds a drilling rotation DOF (rotation about the shell normal) for improved behaviour at shell intersections and junctions.

## Initialisation Steps

1. Read connectivity (3 node IDs, property, material)
2. Compute element normal and local frame
3. Compute initial thickness and area
4. Fill element buffer (`ELBUF`)
5. Compute lumped nodal mass (area × density × thickness / 3)
6. Estimate CFL time step (`dtshell = characteristic_length / wave_speed`)

## Related Documentation

- `engine/source/elements/sh3n/README.md` — engine-side force computation
- `starter/source/elements/README.md` — parent directory overview
- `starter/source/properties/shell/` — shell property types (thickness, integration, etc.)
