# engine/source/ams/

## Purpose
Advanced Mass Scaling (AMS / `/DT/AMS`): adds selective artificial mass to stiff
elements to raise the critical time step without affecting quasi-static or
low-frequency response. AMS builds and solves a sparse mass system each cycle.

## Key files

| File | Role |
|------|------|
| `sms_init.F` | AMS initialization: builds sparse mass matrix, sets up JAD (Jagged Diagonal) storage |
| `sms_build_diag.F` | Builds diagonal mass-scaling entries |
| `sms_build_mat_2.F` | Builds off-diagonal AMS mass entries |
| `sms_pcg.F` | Preconditioned Conjugate Gradient solver for AMS system |
| `sms_proj.F` | Projects AMS forces and accelerations |
| `sms_mass_scale_2.F` | Applies AMS mass scaling to nodal mass array |
| `sms_fixvel.F` | Applies fixed-velocity BCs inside AMS iteration |
| `sms_bcs.F`, `sms_bcs1th.F`, `sms_bcscyc.F` | Boundary condition application in AMS |
| `sms_gravit.F` | AMS gravity load integration |
| `sms_encin_2.F` | AMS kinetic energy estimate |
| `sms_rbe2.F`, `sms_rbe3.F` | RBE2/RBE3 rigid body coupling inside AMS |
| `sms_cjoint.F` | Cylindrical joint constraint in AMS |
| `sms_rgwal*.F` | Rigid-wall interaction inside AMS |
| `sms_rlink.F` | Rigid link in AMS |
| `sms_thbcs.F` | Thermal BCs in AMS |
| `sms_fsa_inv.F` | Full sparse assembly inverse for AMS |
| `sms_admesh.F` | AMS adaptive meshing support |

## Integration in the time loop
AMS is set up during `RESOL` initialization (`SMS_INI_JAD_*`, `SMS_INI_KIN_2`
at line ~2100). Each cycle, the AMS scaled mass replaces the physical nodal mass
(`NODES%MS`) used in `ACCELE` (`CALL DTNODAMS` instead of `CALL DTNODA`).

MPI support: `engine/source/mpi/ams/` — `SPMD_EXCH_NODNX`, `SPMD_EXCH_SMS`,
`SPMD_EXCH_SMS6`, `SPMD_FI_SMS`, `SPMD_VFI_SMS`.

## Dependencies
- Activated by `/DT/AMS` engine keyword
- Called by: `RESOL` (setup in init section, per-cycle in time-step phase)
- Uses `IPARIT` (Parith/ON recommended for AMS for reproducibility)
