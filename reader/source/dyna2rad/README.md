# DYNA2RAD Converter (`reader/source/dyna2rad/`)

Translates LS-DYNA keyword input files to OpenRadioss format: maps DYNA cards to equivalent Radioss cards.

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `dyna2rad/` | Converter implementation: `convertbcs.h`, `convertboxes.h`, `convertcards.h`, `convertconstrainedinterpolations.h`, `convertconstrainedjoints.h`, `convertconstrainednode.h`, `convertconstrainedspotwelds.h`, `convertcontacts.h`, `convertcontrolvols.h`, etc. |

## Architecture

Each `convert*.h` file handles one category of DYNA keywords:
- `convertcards.h` — main keyword dispatcher (maps DYNA keyword → Radioss equivalent)
- `convertcontacts.h` — `*CONTACT_*` → `/INTER` mapping
- `convertbcs.h` — `*BOUNDARY_*` → `/BCS` mapping
- `convertcontrolvols.h` — `*AIRBAG_*` → `/MONVOL` mapping

The converter reads DYNA input via the cfgkernel DYNA reader, then writes Radioss output using the cfgkernel Radioss writer. Translation is table-driven to handle the many keyword variants in each format.

## Related Documentation

- `reader/source/README.md` — reader overview
- `reader/source/cfgkernel/README.md` — shared CFG kernel
