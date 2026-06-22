# TYPE2 Interface — Tied Nodes to Surface (Lagrange) (`starter/source/interfaces/int02/`)

Starter reader for /INTER/TYPE2: Lagrange multiplier tied contact, the most common tied interface type.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type02.F` | Parse /INTER/TYPE2 card parameters |
| `hm_read_inter_lagmul_type02.F` | Parse Lagrange multiplier options for TYPE2 |

## Description

TYPE2 ties slave nodes to the master surface using Lagrange multipliers, providing an exact (not penalised) constraint. The starter reads the slave/master sets, checks geometric proximity, and sets up the constraint pairing. Supports rigid-body to deformable and deformable to deformable coupling.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/int01/README.md` — TYPE1 penalty tied contact
