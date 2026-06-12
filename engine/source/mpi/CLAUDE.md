# engine/source/mpi/

## Purpose
All MPI communication routines for the engine. Every cross-domain data exchange
goes through this directory. The design principle is that all other engine code
calls wrapper functions from `SPMD_MOD` (`spmd_mod.F90`), never raw `MPI_*`
calls directly.

**For the full MPI architecture documentation, see `doc/SPMD_documentation.md`.**

## Quick reference

| Sub-dir | Purpose |
|---------|---------|
| `forces/` | Nodal force exchange: `SPMD_EXCH_A` (Parith/OFF), `SPMD_EXCH2_A_PON` (Parith/ON skyline) |
| `nodes/` | Coordinate/velocity/thickness exchange: `SPMD_SD_XV`, `SPMD_EXCH_THKNOD`, `SPMD_EXCH_WAVE` |
| `interfaces/` | Contact interface exchange: candidate lists, penetrations, stiffnesses |
| `airbags/` | FVM airbag exchange: `SPMD_FVB_SWITCH`, `SPMD_MV_CA`, `SPMD_EXCH_FVSTATS` |
| `ale/` | ALE min/max exchange, neighbor exchange |
| `ams/` | AMS mass/stiffness exchange: `SPMD_EXCH_NODNX`, `SPMD_EXCH_SMS*` |
| `anim/` | Animation data gather: `SPMD_AGET_SECT`, `SPMD_AGETMSR`, `SPMD_ANIM_PLY_*` |
| `elements/` | Element-level exchange: SPH neighbor data, secondary force exchange |
| `fluid/` | CFD/ALE fluid exchange: `spmd_cfd.F` (large file, many routines) |
| `generic/` | Generic allgather/alltoall wrappers |
| `implicit/` | Implicit solver exchange |
| `init/` | MPI initialization: `inipar.F`, `init_global_boundary_list.F90` |
| `kinematic_conditions/` | Rigid body / imposed velocity exchange |
| `lag_multipliers/` | Lagrange multiplier exchange |
| `output/` | Output gather operations |
| `r2r/` | Rank-to-rank specialized exchange |
| `seatbelts/` | Seatbelt force exchange |
| `sections/` | Cross-section force gather |
| `sph/` | SPH particle exchange |
| `user_interface/` | Python element synchronization |

## Key files in root

| File | Role |
|------|------|
| `spmd_mod.F90` | **Umbrella module**: re-exports all MPI wrapper primitives (`spmd_send`, `spmd_recv`, `spmd_allreduce`, etc.) |
| `spmd_comm_world.F90` | `SPMD_COMM_WORLD` communicator constant |
| `spmd_exch_sub.F` | `SPMD_EXCH_SUB_PON`: non-local damage exchange (Parith/ON path) |
| `python_spmd_mod.F90` | Python element state synchronization |
| `get_mpi_operator.F90` | Maps reduction type strings to `MPI_Op` constants |
| `spmd_send.F90` … `spmd_unpack.F90` | Low-level wrappers (blocking send/recv, pack/unpack, allreduce, etc.) |
| `spmd_constants.F90` | MPI constant definitions |
| `spmd_error.F90` | Error checking for MPI calls |

## See also
`doc/SPMD_documentation.md` — detailed explanation of frontier node structures,
Parith/OFF vs. ON force exchange algorithm, IPARIT flag, and NSPMD/ISPMD globals.
