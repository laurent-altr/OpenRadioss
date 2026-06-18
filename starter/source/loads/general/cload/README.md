# Concentrated Load (`starter/source/loads/general/cload/`)

Reads /CLOAD (concentrated nodal load) definitions: point forces and moments applied to node sets.

## Key Files

| File | Role |
|------|------|
| `hm_preread_cload.F` | Pre-read: count /CLOAD blocks, allocate arrays |
| `hm_read_cload.F` | Parse /CLOAD card: force/moment components, function ID, node set |

## Description

`/CLOAD` applies time-varying concentrated forces or moments to individual nodes or node groups. The force magnitude is scaled by a time function (`funct_id`). The starter reads the force vector, function reference, and node set, writing all data to the restart file for the engine load application routines.

## Related Documentation

- `starter/source/loads/general/README.md` — parent directory
- `engine/source/loads/general/README.md` — engine load application
