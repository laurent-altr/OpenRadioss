# engine/source/tools/

## Purpose
Shared utility routines used by many other engine subsystems: function table
interpolation, sensors, cross-sections, seatbelt mechanisms, skew frames,
Lagrange multipliers, accelerometers, and user-callback infrastructure.

## Sub-directories

| Sub-dir | Contents |
|---------|----------|
| `curve/` | Function table and time-curve utilities: `TIMFUN` (evaluates `/FUNCT` tables vs. time), `FINTER`/`VINTER` (linear/vector interpolation), `FINTER_SMOOTH` (smooth interpolant), `LECFUN` (reads function tables), `TABLE_TOOLS` (multi-dimensional table lookup), `TIMFUN` (time-function evaluation each cycle) |
| `sensor/` | Simulation sensors (`/SENSOR`): `SENSOR_BASE` (base sensor check), `SENSOR_ACC` (accelerometer sensor), `SENSOR_AND` (boolean AND sensor), `SENSOR_CONTACT` (contact force sensor), `SENSOR_DIST` / `SENSOR_DIST_SURF` (distance sensor), `STOP_SENSOR` (stop-condition sensor), MPI variants |
| `sect/` | Cross-section force output (`/SECT`): `CUTMAIN` (dispatcher), `CUTCNT`/`CUTCON` (contact cut), `CUTFUNC`/`CUTFUNCE` (function evaluation), `CUTMASS` (mass cross-section), `NORCUT` (normal), `PARCUT` (parallel), `SECTION` (global section force result), `LECCUT` (reader) |
| `skew/` | Local coordinate systems (`/SKEW`): `NEWSKW` (builds skew frame), `MOVFRAM` (moving frame), `RELFRAM`/`RELFRAM_M1` (relative frame), `ROTBMR` (rotation update) |
| `seatbelts/` | Seatbelt mechanism: `KINE_SEATBELT_FORCE`/`KINE_SEATBELT_VEL` (force/velocity update), `GUIDED_CABLE_FORCE`, `RETRACTOR_TABLE_INV`, `MATERIAL_FLOW`, `SEATBELT_REDUCTION_FACTOR`, `REDEF_SEATBELT` |
| `lagmul/` | Lagrange multiplier constraints: `LAG_DIRECT` (direct solve), `LAG_BCS` (BCs), `LAG_FXV` (fixed velocity), `LAG_GJNT` (gear joint), `LAG_ANITH` (arithmetic), `LAG_I2MAIN` (type 2 interface Lagrange), `GJNT_GEAR`/`GJNT_DIFF`/`GJNT_RACK` (gear kinematics) |
| `accele/` | Accelerometer: `ACCEL1` (computes nodal acceleration history for `/TH/ACCEL` output), `CHOLFACT` (Cholesky for accelerometer filter) |
| `univ/` | Universal utilities: `BUTTERWORTH` (Butterworth signal filter), `INICOD` (code flag init), `ISTR`/`RSTR`/`STRI`/`STRR` (integer/real ↔ string conversion) |

## Files in root `tools/`

| File | Role |
|------|------|
| `finter_mixed.F90` | Mixed-precision function interpolation |
| `dyn_userlib.c`, `dyn_userlib_callback.c` | Dynamic user library loading (for user-defined materials/elements) |
| `eng_callback_c.c` | C-side callback registration for user routines |
| `interface_utable.F` | User table interface registration |
| `utable.F` | User table dispatch (`UTABLE` callback driver) |
| `uintbuf_mod.F` | User interface buffer module |
| `uaccess.F` | User access point for element data |
| `ufunc.F` | User function callback |
| `usensor.F` | User-defined sensor callback |
| `usrplas.F` | User plasticity (legacy `UPLAS` callback) |
| `user_output.F` | User-defined output callback |
| `user_windows.F` | User windows (Altair HyperWorks specific) |
| `userwindow_interface_routines.F` | Interface for user window callbacks |
| `ushforce3.F90` | User shell force (3-node) callback |
| `upidmid.F` | User property/material ID lookup |
| `suforc3.F` | User solid force callback entry point |
| `nolib_usermat99.F` | No-library stub for user material 99 |

## Dependencies
- `curve/timfun.F`: called from `RESOL` at start of each cycle to evaluate time functions
- `sensor/stop_sensor.F`: called from `RESOL` Step 0 to check stop conditions
- `sect/cutmain.F`: called from `SORTIE_MAIN` for cross-section output
- User callbacks: loaded dynamically via `dyn_userlib.c` when `/ULIB` keyword is present
