# Rivet Element (`engine/source/elements/rivet/`)

A single-file module implementing the rivet connector element force computation.

## Key File

| File | Role |
|------|------|
| `rivet1.F` | Rivet element force computation: axial and shear stiffness between two connected nodes |

## Rivet Element

The rivet (`/RIVET`) is a two-node connector element modelling bolted or riveted joints. It:
- Provides axial stiffness (tension/compression)
- Provides shear stiffness in the transverse plane
- Supports optional failure (pull-out force, shear force limits)

When failure is triggered, the rivet ceases to transmit force (element is deactivated).

The time step for a rivet is governed by `sqrt(m/K)` where `m` is the associated nodal mass and `K` is the stiffness.

## Related Documentation

- `engine/source/elements/README.md` — parent elements directory overview
- `starter/source/elements/reader/rivet0.F` — rivet HM reader in starter
