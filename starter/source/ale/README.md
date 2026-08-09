# Starter ALE Subsystem

This subsystem reads and initialises ALE and Euler model definitions.

## Key Files and Subdirectories

| Path | Role |
|------|------|
| `alelec.F` | Top-level ALE keyword reader |
| `alesop.F` | ALE solid-of-porous material setup |
| `ale_check_lag.F` | Check Lagrangian elements adjacent to ALE domain |
| `atheri.F` | ALE thermal coupling parameter reading |
| `athlen.F` | ALE thermal element length computation |
| `ale2d/` | 2D ALE mesh initialisation |
| `ale3d/` | 3D ALE mesh initialisation |
| `bimat/` | Bi-material (two-fluid) interface initialisation |

## ALE Initialisation Steps

1. **Mesh identification**: Determine which elements are ALE/Euler (vs. Lagrangian)
2. **Boundary setup**: Identify ALE domain boundaries (inflow, outflow, wall)
3. **Initial material state**: Assign material to each ALE cell (volume fraction for multi-material)
4. **Grid parameters**: Store rezoning strategy, advection scheme selection
5. **Thermal coupling**: If thermo-mechanical, set up thermal ALE parameters

## Interface with Standard Elements

ALE elements are standard solid elements with additional material state (volume fractions, mesh velocity DOF). The starter marks these elements as ALE type in the restart file and stores the extra state variables.

The `ale_check_lag.F` routine validates that Lagrangian elements adjacent to the ALE domain are not erroneously included in the ALE mesh.

## Related Documentation

- `engine/source/ale/README.md` — runtime ALE computation
- `starter/source/initial_conditions/README.md` — `/INIVOL` for initial volume fractions
