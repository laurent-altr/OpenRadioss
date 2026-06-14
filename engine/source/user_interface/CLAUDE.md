# engine/source/user_interface/

## Purpose
User-defined material, element, and sensor extension interface. Allows users to
plug in external Fortran or C subroutines (via a shared library) that are called
by the engine during material-law evaluation, element force computation, or
sensor checking.

## Files

| File | Role |
|------|------|
| `dyn_userlib.c` (in `tools/`) | Dynamically loads the user shared library at startup via `dlopen` |
| `dyn_userlib_callback.c` (in `tools/`) | Registers user function pointers into the engine callback table |
| `eng_callback_c.c` (in `tools/`) | C-side callback invocation wrappers |
| `utable.F` (in `tools/`) | Fortran `UTABLE` dispatch: called from material kernels when material law = 99 (user material) |
| `uintbuf_mod.F` (in `tools/`) | Module `UINTBUF_MOD`: buffer type for passing element state to user routines |
| `suforc3.F` (in `tools/`) | Entry point for user solid element force callback |
| `ushforce3.F90` (in `tools/`) | Entry point for user shell (3-node) element force callback |
| `usrplas.F` (in `tools/`) | Entry point for user plasticity law (legacy `UPLAS` interface) |
| `usensor.F` (in `tools/`) | Entry point for user-defined sensor callback |
| `user_output.F` (in `tools/`) | Entry point for user-defined output callback |
| `nolib_usermat99.F` (in `tools/`) | Stub used when no user library is loaded (prints error and stops) |
| `upidmid.F` (in `tools/`) | Translates user-facing PID/MID to internal indices for user callbacks |
| `uaccess.F` (in `tools/`) | Provides user read/write access to element and nodal data |
| `ufunc.F` (in `tools/`) | User function table lookup callback |
| `user_windows.F` (in `tools/`) | HyperWorks user window integration |
| `interface_utable.F` (in `tools/`) | Fortran interface declarations for `UTABLE` |

## How user materials/elements work

1. The user creates a shared library implementing `UTABLE`, `UPLAS`, `SUFORC3`, etc.
2. At startup, `/ULIB` keyword in the engine deck specifies the library path.
3. `dyn_userlib.c` loads the library with `dlopen`; `dyn_userlib_callback.c` resolves
   function pointers.
4. Inside the time loop, when an element with `ILAW=99` (user material) is reached,
   the element kernel calls `UTABLE(…)` which dispatches to the user function pointer.

## Dependencies
- Called by: material kernels in `engine/source/materials/` when `ILAW=99`
- Called by: element kernels when `IPARG(5,NG) = user element type`
- Uses: `engine/source/tools/dyn_userlib.c` for dynamic library loading
