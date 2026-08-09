# Interface Buffer Utilities (common_source/interf)

This directory provides the shared **interface buffer** data structures and utilities used by both the starter and the engine for contact interface management.

## Key Data Structure: `INTBUF_DEF_MOD`

The central export is the `intbuf_struct` type defined in `intbuf_ini.F`. This structure holds the integer metadata arrays for a single contact interface:

- Node lists (secondary nodes, primary segment node indices)
- Status flags (active/inactive, eroded)
- Sorting indices and candidate pair bookkeeping
- Interface type identifier and configuration integers

This structure is passed as a dummy argument to virtually all interface-handling routines in `engine/source/interfaces/`.

## Key Files

| File | Role |
|------|------|
| `intbuf_ini.F` | Define and initialise `intbuf_struct` |
| `copy_intbuf_tab.F` | Deep-copy an interface buffer (for restart / domain decomposition) |
| `i2cin_rot27.F` | Rotation computation for TYPE27 interface buffer |
| `i2loceq.F` | Local equilibrium computation for tied interfaces |
| `i2pen_rot.F` | Penetration computation with rotation correction |
| `i2rep.F` | Repair penetrations in interface buffer |
| `upgrade_multimp.F` | Upgrade buffer for multi-point constraints |
| `nearest_seg.F` | Find nearest primary segment to a secondary node |
| `int18_law151_nsv_shift.F` | NSV (nodal state variable) index shift for TYPE18/LAW151 |

## Usage Pattern

The buffer is initialised by the starter (`starter/source/interfaces/`) and written into the restart file. The engine reads it back and passes it to the interface routines at each time step:

```fortran
use INTBUF_DEF_MOD, only : intbuf_struct

type(intbuf_struct), intent(inout) :: intbuf_tab   ! per-interface buffer
```

Each interface type (`int07/`, `int25/`, etc.) receives its own `intbuf_tab` and uses the integer arrays within it to track candidate pairs, penetration flags, and force history.

## MPI Considerations

When the domain is decomposed, secondary nodes and primary segments may reside on different MPI ranks. The buffer contains the information needed to exchange data across rank boundaries. `copy_intbuf_tab.F` is used during domain decomposition to copy buffer slices to ghost ranks.

## Related Documentation

- `engine/source/interfaces/README.md` — uses these buffers in all contact types
- `common_source/modules/` — `INTBUF_DEF_MOD` module definition
