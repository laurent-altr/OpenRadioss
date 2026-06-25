# engine/source/airbag/

## Purpose
Finite-volume (FV) airbag model: inflator gas injection, bag pressure/volume
computation, venting, porosity, rezoning, and time-step control. Called from
`RESOL` as part of the external-force step (before element force loops).

## Entry points

| Routine | File | Role |
|---------|------|------|
| `MONVOL0` | `monvol0.F` | Dispatcher for monitored-volume airbag laws (TYPE 1–10) |
| `FVBAG0` | `fvbag0.F` | FV airbag initialization |
| `FVBAG1` | `fvbag1.F` | FV airbag time step |
| `FVBAG2` | `fvbag2.F` | FV airbag force assembly |
| `AIRBAG1`/`AIRBAG2` | `airbag1.F`, `airbag2.F` | Control-volume airbag models |
| `AIRBAGA1`/`AIRBAGB1` | `airbaga1.F`, `airbagb1.F` | Airbag A/B variants |
| `PORFOR4/5/6` | `porfor4.F`…`porfor6.F` | Porous fabric flow force computation |
| `MVOLUDT` | `mvoludt.F` | Monitored-volume time-step contribution |
| `FVREZONE` | `fvrezone.F` | FV mesh re-centering after large deformation |
| `FVVENT0` | `fvvent0.F` | Vent flow model |
| `FVSTATS`/`FVSTATS1` | `fvstats.F`, `fvstats1.F` | FV bag statistics output |
| `RBAGDT` | `rbagdt.F` | Retainer bag time step |
| `VOLPRES`/`VOLPRESP` | `volpres.F`, `volpresp.F` | Volume/pressure update for CV bags |
| `SMS_*` | `sms_*.F` | Sub-meshed surface (SMS) routines for FV bags |

## Key data
- Airbag state uses monitored-volume structures from `common_source/modules/` (FVM arrays).
- MPI exchange: `SPMD_FVB_SWITCH`, `SPMD_MV_CA`, `SPMD_EXCH_FVSTATS` in `engine/source/mpi/airbags/`.
- `INIT_GLOBAL_MONVOL_FRONTIER` builds cross-domain bag connectivity.
- Control-volume bags use a simple pressure-volume law; FV bags use a 3D FVM mesh.

## Dependencies
- Called by: `RESOL` (`engine/source/engine/resol.F`)
- Uses: `common_source/modules/` airbag types, `engine/source/mpi/airbags/`
