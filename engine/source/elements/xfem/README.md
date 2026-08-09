# XFEM Force Computation (`engine/source/elements/xfem/`)

Computes internal forces for XFEM-enriched elements and propagates cracks each time step.

## Key Files

| File | Role |
|------|------|
| `allocxfem.F` | Allocate XFEM working arrays for current time step |
| `cbufxfe.F` | Update XFEM element buffer with current enrichment state |
| `fillcne_xfem.F` | Update crack normal and enriched node connectivity |
| `iedge_xfem.F` | Update edges intersected by crack front |
| `lslocal.F` | Recompute level-set values at nodes (level-set advection) |
| `precrkxfem.F` | Pre-process crack: update front position from propagation criterion |
| `preinicrk3N.F` | Reinitialise crack data for 3-node elements after propagation |
| `preinicrk4N.F` | Reinitialise for 4-node elements |
| `pretag_xfem.F` | Re-tag elements (enriched/tip/unenriched) after crack advance |
| `thick_ilev.F` | Update thickness integration levels for cracked shell elements |
| `xfem_crack_init.F` | Reinitialise XFEM data for elements newly entered by the crack |
| `inicrkfill.F` | Write updated crack state to output |

## Crack Propagation Algorithm

Each time step:
1. **Level-set update** (`lslocal.F`): advect level-set field with crack velocity
2. **SIF computation**: compute stress intensity factors `K_I`, `K_II` at crack tip elements
3. **Propagation criterion**: check if `K_eff > K_IC` (Paris law or similar)
4. **Crack advance** (`precrkxfem.F`): extend crack front by `da` in computed direction
5. **Re-tagging** (`pretag_xfem.F`): update element classifications
6. **Force computation**: enriched elements compute additional DOF contributions (Heaviside and tip enrichment functions)

## Related Documentation

- `starter/source/elements/xfem/README.md` — XFEM initialisation
- `engine/source/elements/README.md` — parent elements overview
- `common_source/fail/README.md` — Newman-Raju SIF formula used by XFEM
