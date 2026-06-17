# Element Buffer Management (`engine/source/elements/elbuf/`)

Manages element buffer (`ELBUF`) allocation, copying, and I/O for the engine.

## Key Files

| File | Role |
|------|------|
| `elbuf_ini.F` | Initialise the `ELBUF` structure — set array pointers and sizes for each element group |
| `allocbuf_auto.F` | Allocate `ELBUF` arrays from the restart file metadata |
| `alloc_elbuf_imp.F` | Allocate additional `ELBUF` storage needed by the implicit solver |
| `copy_elbuf.F` | Deep copy of `ELBUF` (used for restart checkpointing) |
| `copy_elbuf_1.F` | Copy one element group's buffer (component of `copy_elbuf.F`) |
| `w_elbuf_str.F` | Write `ELBUF` structure to restart file |

## ELBUF Structure

`ELBUF` (Element Buffer) is a flat array that stores all per-element state for one element group:
- Geometric data: nodal coordinates, Jacobian, thickness
- Material state: stress tensor, back-stress, plastic strain
- EOS state: density, internal energy, pressure
- Failure flags and damage variables

Each element group (defined by a common element type + property + material combination) has its own contiguous ELBUF slice. This layout is cache-friendly for the inner force-computation loops.

## Engine vs Starter

The starter allocates and writes `ELBUF` to the `_0001.rad` restart file. The engine reads and initialises from that file using `allocbuf_auto.F` and `elbuf_ini.F`. At each time step, the engine modifies `ELBUF` in-place (stress update, history variable update).

## Related Documentation

- `engine/source/elements/README.md` — parent elements directory overview
- `starter/source/elements/elbuf_init/README.md` — starter-side ELBUF initialisation
