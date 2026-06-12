# modules/ale/

## Purpose
Data structures and parameters for the Arbitrary Lagrangian-Eulerian (ALE) method: connectivity, boundary conditions, MUSCL scheme buffers, and multi-material FVM.

## Files

| File | Module | Description |
|------|--------|-------------|
| `ale_mod.F` | `ALE_MOD` | Core ALE data structures and global parameters |
| `ale_connectivity_mod.F` | `ALE_CONNECTIVITY_MOD` | ALE element connectivity structures for mesh management |
| `ale_ebcs_mod.F` | `ALE_EBCS_MOD` | Defines `NEBCS` integer and element boundary condition structures for ALE |
| `alemuscl_mod.F` | `ALEMUSCL_MOD` | Types `ALEMUSCL_BUFFER_` (volume fraction, gradients, node data) and `ALEMUSCL_PARAM_` (MUSCL scheme parameters for high-order advection) |
| `multi_fvm_mod.F90` | `MULTI_FVM_MOD` | Multi-fluid / multi-material FVM data structures |

## Key Types Exported
- **`ALEMUSCL_BUFFER_`** — per-cell buffers for MUSCL advection (volume fraction, gradient arrays)
- **`ALEMUSCL_PARAM_`** — MUSCL scheme configuration parameters

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: engine ALE solver routines
