# modules/airbag/

## Purpose
FVM (Finite Volume Method) airbag mesh control data: global arrays for mesh nodes, solids, and temporary working buffers used during airbag inflation computation.

## Files

| File | Module | Description |
|------|--------|-------------|
| `fvmbag_meshcontrol_mod.F` | `FVMBAG_MESHCONTROL_MOD` | Global arrays for FVM airbag mesh: `KMESH` (mesh control), `NODE_COORD` (node coordinates), `IXS_TEMP` (solid connectivity buffer), `IBUFSSG_TEMP` (segment buffer) |

## Key Types Exported
- **`FVMBAG_MESHCONTROL_MOD`** — module-level global arrays (no derived type; data stored directly as module variables)

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: engine FVM airbag routines
