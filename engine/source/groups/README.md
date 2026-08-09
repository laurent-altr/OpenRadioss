# Groups Subsystem

This subsystem manages node and element group data structures in the engine. Groups (also called sets) are collections of entities used to apply loads, boundary conditions, output requests, and contacts.

## Group Types

| OpenRadioss keyword | Entity type | Engine group type |
|--------------------|-------------|------------------|
| `/GRNOD` | Node group | `NODGRP` |
| `/GRSH` | Shell element group | `SHGRP` |
| `/GRSO` | Solid element group | `SOGRP` |
| `/GRBE` | Beam element group | `BEGRP` |
| `/GRSP` | Spring element group | `SPGRP` |
| `/GRTRIA` | Triangular shell group | `TRIGRP` |
| `/GRPART` | Part (property-based) group | `PARTGRP` |

Groups are defined in the starter and stored in the restart file; the engine reads them and uses them for load/BC/output dispatch.

## Key Files

| File | Role |
|------|------|
| `alloc_group_str.F` | Allocate the group data structure arrays |
| `alloc_line_str.F` | Allocate line (segment) group structures |
| `alloc_surf_str.F` | Allocate surface group structures |
| `alloc_subset_str.F` | Allocate subset group structures |
| `group_ini.F` | Initialise group data from restart |
| `line_ini.F` | Initialise line group data |
| `surf_ini.F` | Initialise surface group data |
| `subset_ini.F` | Initialise subset data |

## Group Usage

Groups are referenced by most other subsystems:

- **Loads**: `/CLOAD`, `/PLOAD`, `/GRAV` target node or element groups
- **Boundary conditions**: `/BCS`, `/EBCS` target node groups
- **Output**: `/TH/NODA`, `/TH/SHEL`, etc. select entity groups for time history output
- **Interfaces**: Contact interfaces reference surface groups (master / secondary)
- **Materials / Properties**: `/GRPART` selects elements by part ID for grouped operations

## Surface and Line Groups

Beyond element and node groups, OpenRadioss supports:
- **Surface groups** (segment sets): collections of element faces — used for pressure loads and contact surfaces
- **Line groups**: collections of element edges — used for edge-to-edge contact

These are managed by `alloc_surf_str.F` / `surf_ini.F` and `alloc_line_str.F` / `line_ini.F` respectively.

## Related Documentation

- `engine/source/loads/README.md` — groups used as load targets
- `engine/source/interfaces/README.md` — groups used as contact surfaces
- `engine/source/output/README.md` — groups used in time history output requests
