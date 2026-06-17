# Seatbelt Elements (`starter/source/tools/seatbelts/`)

Reads and initialises seatbelt and guided-cable elements — 1D elements that slide through rings and retractors.

## Key Files

| File | Role |
|------|------|
| `ini_seatbelt.F` | Main seatbelt initialisation: create element connectivity, assign to rings/retractors |
| `new_seatbelt.F` | Allocate and construct a new seatbelt element group |
| `create_seatbelt.F` | Create seatbelt elements from node path definition |
| `init_seatbelt_rbodies.F90` | Initialise rigid-body connection for seatbelt anchorage nodes |
| `hm_read_retractor.F` | HM binary reader for `/RWALL/RETRACTOR` |
| `hm_read_slipring.F` | HM binary reader for `/SEATBELT/SLIPRING` |
| `hm_read_guided_cable.F90` | HM reader for guided cable definition |
| `ini_guided_cable.F90` | Initialise guided cable from HM data |
| `hm_print_inter_guided_cable.F90` | Print guided cable interface data (validation output) |
| `find_prev_next_nodes.F90` | Find preceding/following nodes along seatbelt path |
| `dist_node_segment.F90` | Compute distance from node to seatbelt segment (for ring slip computation) |

## Seatbelt System

A seatbelt in OpenRadioss consists of:
- **Belt elements**: 1D elements along the belt path (similar to bar/truss elements)
- **Slip rings**: contact rings where the belt can slide through (webbing path changes)
- **Retractors**: reel mechanisms with load-limiter and locking behaviour

The starter creates belt element connectivity from the defined node path, assigns slip rings and retractors, and writes the complete seatbelt topology to the restart file.

## Related Documentation

- `starter/source/tools/README.md` — parent tools directory
- `engine/source/tools/README.md` — seatbelt force computation in engine
