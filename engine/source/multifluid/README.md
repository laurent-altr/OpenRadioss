# Multi-Fluid Subsystem

This subsystem implements multi-fluid (multi-material) ALE models where several distinct fluid species coexist within the same Eulerian mesh. Each cell may contain a mixture of fluids tracked by volume fractions.

Activated by `/MULTIFLUID` and related keywords.

## Key Files

| File | Role |
|------|------|
| `multi_allocate.F` | Allocate multi-fluid data structures |
| `multi_deallocate.F` | Deallocate multi-fluid arrays |
| `multi_evolve_global.F` | Main time-step driver for multi-fluid evolution |
| `multi_compute_dt.F` | Time step control for multi-fluid stability |
| `multi_computevolume.F` | Compute volume fractions per cell per fluid |
| `multi_bilan.F` | Mass/energy balance check across all fluid species |
| `multi_buf2var.F` | Copy multi-fluid buffer arrays to working variables |
| `multi_ebcs.F` | Apply EBCS boundary conditions to multi-fluid domain |
| `centroid.F` | Compute centroid positions for interface tracking |
| `connectivity.F` | Multi-fluid mesh connectivity management |

## Physical Model

In a multi-fluid ALE cell, multiple material species share the cell volume:

```
Σ α_k = 1    (volume fractions sum to 1)
```

where `α_k` is the volume fraction of species `k`. Each species carries its own density, pressure, and internal energy. A pressure equilibrium condition is enforced at each time step to ensure that all species in a cell reach the same pressure.

## Typical Applications

- Air-water interactions (sloshing, underwater explosions)
- Explosive products mixed with inert gas
- Spray / droplet-laden flows
- Multi-phase compressible flows

## Relation to ALE and EOS

The multi-fluid subsystem operates on top of the ALE infrastructure (`ale/`). Each fluid species uses its own EOS (`common_source/eos/`) to compute pressure from density and energy. The ALE advection step (`ale/alemuscl/`) is performed separately for each species' conserved quantities.

## Related Documentation

- `engine/source/ale/README.md` — underlying ALE/Euler framework
- `common_source/eos/README.md` — EOS models for each fluid species
