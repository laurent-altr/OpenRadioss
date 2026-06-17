# Domain-Decomposition Restart Split (`starter/source/restart/ddsplit/`)

Writes per-domain restart files after METIS domain decomposition: the full model is split across `N` files (`_0001.rad`, `_0002.rad`, …) for parallel engine startup.

## Key Files

| File | Role |
|------|------|
| `ddsplit.F` | Master split routine: distribute model data to per-domain restart files |
| `wrrest.F` | Write one domain's restart data |
| `wrcomm.F` / `wrcommp.F` | Write communication data (ghost node maps, send/receive lists) |
| `w_*.F` | Per-entity writers: `w_geo.F` (geometry), `w_elbuf_str.F` (element buffer), `w_group_str.F` (groups), `w_rnloc.F` (node-local), etc. |
| `c_*.F` | Count routines: count data size before allocation for each entity type |
| `split_interfaces.F` | Split contact interface data across domains |
| `split_bcs_nrf.F90` | Split NRF boundary condition data |
| `split_bcs_wall.F90` | Split rigid wall data |
| `split_skew.F` | Split SKEW frame data |

## Architecture

After METIS assigns each element to a domain, `ddsplit.F` iterates over domains and for each:
1. Identifies owned elements and their nodes (including ghost nodes on domain boundaries)
2. Calls `c_*.F` routines to count data sizes and allocate buffers
3. Calls `w_*.F` routines to fill and write per-domain restart file
4. Writes communication tables (which ghost nodes each domain sends to / receives from neighbours)

The per-domain files are read independently by each MPI rank at engine startup (`rdresa.F`/`rdresb.F` in `engine/source/output/restart/`).

## Related Documentation

- `starter/source/spmd/domain_decomposition/README.md` — METIS decomposition
- `engine/source/output/restart/README.md` — engine restart reader
