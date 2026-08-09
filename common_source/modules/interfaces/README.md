# Interface Modules (`common_source/modules/interfaces/`)

Fortran 90 modules defining data types and arrays for the contact/interface subsystem. Shared between starter (contact initialisation) and engine (contact force computation).

## Modules

| File | Module | Contents |
|------|--------|---------|
| `intbufdef_mod.F90` | `INTBUF_DEF_MOD` | `INTBUF_STRUCT` type definition — the per-contact-pair buffer (normal gap, tangential slip, friction state, penetration depth) |
| `interfaces_mod.F90` | `INTERFACES_MOD` | Global interface arrays: `INTBUF_TAB` (array of contact buffers), interface type flags |
| `int8_mod.F90` | `INT8_MOD` | TYPE8 (tied surface-to-surface) specific data |
| `intbuf_fric_mod.F90` | `INTBUF_FRIC_MOD` | Friction state arrays: accumulated tangential slip, Coulomb flag |
| `metric_mod.F` | `METRIC_MOD` | Metric tensor for surface normal computation |
| `parameters_mod.F` | `PARAMETERS_MOD` | Contact algorithm parameters: search radius, tolerance, penalty factors |
| `spmd_arrays_mod.F` | `SPMD_ARRAYS_MOD` | MPI-distributed contact arrays: ghost segment lists, cross-domain contact pairs |
| `th_surf_mod.F` | `TH_SURF_MOD` | Time history surface data for contact force output |

## INTBUF_STRUCT

The most important type in this directory. Each contact pair (node + segment) has one `INTBUF_STRUCT` instance storing:
- Current gap in normal direction
- Accumulated tangential relative displacement (for friction)
- Friction state (sliding / sticking)
- Penetration velocity

This is allocated by the starter and grown dynamically by the engine as new contact pairs form.

## Related Documentation

- `common_source/interf/README.md` — lower-level interface buffer operations
- `common_source/modules/README.md` — parent modules directory
- `engine/source/interfaces/README.md` — contact force computation
