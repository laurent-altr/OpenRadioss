# GPU offload of 4-node shells with material law 2 (Johnson-Cook)

This document describes the CUDA offload of the internal-force computation for
4-node shells using material law 2, and records the findings of the
investigation into the **elemental time step** (2026-07).

## Files

| File | Role |
|---|---|
| `shell_gpu_driver.cu` | Device memory management, H2D/D2H transfers, kernel launches, min-dt reduction kernel |
| `shell_geometry_kernel.cu` | Kernel 1 (K1): CCOOR3 + CNVEC3 + CDERI3 + CDLEN3 (ALDT²) |
| `shell_strain_material_kernel.cu` | Kernel 2 (K2): CCOEF3 + CDEFO3 + CCURV3 + CSTRA3 + SIGEPS02C/M2CPLR |
| `shell_force_assembly_kernel.cu` | Kernel 3 (K3): CHVIS3 + CFINT3 + CUPDT3 (atomicAdd assembly) + STI/STIR for STIFN |
| `shell_gpu_data.h` | `ShellGPUData` (per super-group) and `ShellGPUGlobal` (shared node/force arrays) |
| `shell_gpu_mod.F90` | Fortran `bind(c)` interfaces + `SHELLS_` / `GPU_SHELL_LAW2` host mirror types |
| `shell_internal_forces.F90` | `FORINTC_PREPARE_GPU` (init), `gpu_shell_launch_async`, `gpu_shell_sync_scatter` |

Build: `.cu` files are compiled only with the NVIDIA HPC SDK toolchain
(`WITH_CUDA` is then defined, see `engine/CMakeLists.txt`). At run time the
offload is enabled with the engine command-line flag `-gpu`
(`execargcheck.F` sets `GPU=1`).

## Workflow

### Initialization (once, before the cycle loop)

`resol.F` calls `FORINTC_PREPARE_GPU` which:

1. Scans element groups and selects the offloadable ones
   (ITY=3, MLW=2, NPT>0, JHBE<11, IGTYP<29, no XFEM/failure/section/
   slipring/seatbelt/substack), merging compatible groups (same NPT, ISMSTR,
   JHBE, ITHK, IPLA) into **super-groups (SU)**.
2. Creates the shared `ShellGPUGlobal` handle (device X/V/VR `[9*NUMNOD]` and
   force buffer Fx..STIFR `[8*NUMNOD]`, one CUDA stream + `upload_done` event),
   pins `NODES%X/V/VR` and the host D2H buffer.
3. Per SU: allocates device arrays, uploads connectivity, thickness, OFF,
   per-element material scalars, and per-integration-point state
   (sig, pla, epsd, back-stress, temperature) from `ELBUF_TAB`.
4. Sets `compute_sti` and `reduce_elem_dt` from the time-step configuration
   (see "Element time step" below).

The corresponding groups are **skipped by `FORINTC`** (CYCLE) when `GPU==1`,
so the GPU fully replaces the CPU chain for them — including the time step.

### Per cycle

```
resol.F
  gpu_shell_launch_async(NODES, SHELLS, DT1)        ! async, returns immediately
    STEP 1  global H2D of X/V/VR, zero force arrays, record upload_done
    STEP 2  per SU: stream waits upload_done, enqueue K1 -> K2 -> K3,
            record kernels_done
    STEP 3  global stream waits all kernels_done, enqueue single D2H of
            the [8*NUMNOD] force buffer
    STEP 4  per SU (if reduce_elem_dt): enqueue min-dt reduction kernel
            + 8-byte D2H into SHELLS%LAW2(SU)%dt_min_result
  ... INTFOP8 / FORINTC / FORINT run on the CPU meanwhile ...
  gpu_shell_sync_scatter(NODES, SHELLS, DT2T, ...)
    STEP 1  synchronize global stream + each SU stream
    STEP 2  scatter Fx..Mz into NODES%A/AR, STIFN/STIFR into NODES%stifn/stifr
    STEP 3  dt_gpu_min = min over SUs of dt_min_result
    STEP 4  if (reduce_elem_dt .and. dt_gpu_min > 0 .and. dt_gpu_min < dt2t)
              dt2t = dt_gpu_min
```

## Element time step: CPU reference vs GPU

CPU chain for an offloaded group (`cforc3.F`):

1. `CDLEN3` — characteristic length: `ALDT² = max(AREA²/max(AL5,AL6), ALMIN)`
   with hourglass correction `min(ALDT², 0.5*(ALMIN+ALDT²)/max(H1,H2))`,
   then `ALDT = sqrt(ALDT²)`.
2. `CDT3` (called when `ISMSTR/=3 .AND. (NODADT==0 .OR. IDTMIN(3)/=0)`):
   - standard branch: `DT = DTFAC1(3) * ALDT * VISCMX/sqrt(ALPE) / SSP`.
     For law 2, `VISCMX=0` out of the material, transformed in `cforc3`
     to `sqrt(1+0)-0 = 1`, and `ALPE = 1` (`ccoef3.F`) — so effectively
     `DT = DTFAC1(3)*ALDT/SSP` with `SSP = PM(27,imat)`.
   - returns **without** touching DT2T when `NODADT==0 .AND. IDTMIN(3)==0`
     or when `NODADT/=0`; otherwise reduces `DT2T` (and sets
     `NELTST`/`ITYPTST=3`).
   - Note: with `NODADT==0`, `lectur.F` defaults `IDTMIN(3)=2`, so in a
     standard element-timestep run the reduction **always** happens on CPU.

GPU chain (functionally equivalent for the standard case):

1. K1 stores `ALDT²` in `d_STI[]` (exact CDLEN3 formula, squared form).
   K3 only *reads* `d_STI` (for the `compute_sti==2` STIFN formula), so the
   value is still valid afterwards.
2. `shell_min_dt_kernel` computes
   `dt = DTFAC1(3) * sqrt(ALDT²) / SSP` for elements with `OFF>0`, block-level
   tree reduction + `atomicMin`, and an 8-byte D2H of the scalar result.
3. `gpu_shell_sync_scatter` reduces `DT2T` with the min over super-groups.

Gating (`FORINTC_PREPARE_GPU`), mirroring `CDT3`:

| Configuration | `compute_sti` | STIFN source (K3) | `reduce_elem_dt` |
|---|---|---|---|
| `NODADT/=0` or `IDT1SH==1` or `IDTMINS==2` | 1 | CHVIS3/CHSTI3 formula | false |
| `NODADT==0 .AND. IDTMIN(3)/=0` (standard element dt) | 2 | CDT3 formula `0.81*0.5*VOL0*YM/ALDT²` | **true** |
| `NODADT==0 .AND. IDTMIN(3)==0` (cannot happen after lectur defaults) | 0 | none | false |

## Findings of the time-step investigation (2026-07)

### Bug found and fixed: `-gpu` on a non-CUDA build silently ran no-op stubs

This one reproduces the broken element time step on **any** deck — with or
without contact interfaces.

The `.cu` files are compiled **only** when the CMake variable `gpu_cc` is
defined, which only the `cmake_linux*_nvidia.txt` toolchains do. With any
other toolchain (gfortran, ifx, ifort, AOCC) the CUDA sources are silently
dropped and `shell_gpu_mod.F90` links its `#else` branch: **empty stub
subroutines**. The stub header claimed *"they are never called at runtime
because the GPU code path is guarded by the gpu_shell_available flag"* —
but **no such flag existed anywhere**. Running such an executable with
`-gpu`:

- `FORINTC_PREPARE_GPU` runs normally (pure Fortran) and `FORINTC`
  **skips every offloaded shell group** (the skip only tests the `-gpu`
  command-line flag, not the build),
- all `shell_gpu_*` calls are no-ops: no forces, and
  `dt_min_result` is never written → `DT2T` collapses to `0.0`
  (before the hardening below) on the very first cycle,
- deceptively, all the Fortran-side `[GPU-CFG]`/`[GPU]` prints still
  appear — only the C-side `[GPU-GLOBAL] Created global handle` banner
  is missing. **Check for that banner to know whether the build really
  has CUDA.**

Fixes applied:

1. `shell_gpu_mod.F90`: the `gpu_shell_available` logical parameter now
   actually exists (`.true.` only when `WITH_CUDA` is defined).
2. `resol.F`: before `FORINTC_PREPARE_GPU`, if `-gpu` was requested on a
   non-CUDA executable, a warning is written to the listing and stdout and
   `GPU` is reset to 0 — the run falls back to the full CPU path instead
   of silently producing zero forces and a zero time step.

### Bug found and fixed: launch was nested inside `IF(NINTER/=0)`

`CALL gpu_shell_launch_async` in `resol.F` sat **inside** the
`IF(NINTER/=0)` contact block (after `INTFOP8`), while
`gpu_shell_sync_scatter` runs unconditionally. In any model **without
contact interfaces** (`NINTER==0` — typical for law-2 validation decks):

- the three kernels and the min-dt reduction were never enqueued,
- the shells produced **zero forces** (the D2H buffer stays zeroed),
- `dt_min_result` kept its initial value `0.0`, so `gpu_shell_sync_scatter`
  forced `DT2T = 0.0` on the first cycle → "elemental time step not
  working".

So neither a missing D2H nor a missing timestep computation: the whole GPU
pipeline (including the existing, correct min-dt download) was simply never
launched on those decks. Fixes applied:

1. `resol.F`: launch moved out of (and before) the `IF(NINTER/=0)` block.
2. `shell_internal_forces.F90`: DT2T reduction now also requires
   `dt_gpu_min > 0`, so an unset result can never zero the time step.
3. `shell_gpu_mod.F90`: `dt_min_result` initialized to `1.0d30` instead
   of `0.0`.

### Verified correct (no action needed)

- K1 `ALDT²` matches `CDLEN3` exactly (incl. squared hourglass correction).
- min-dt formula matches `CDT3` for law 2 (`VISCMX→1`, `ALPE=1`,
  `SSP = PM(27)` on both sides).
- K3 does not overwrite `d_STI` before the min-dt kernel reads it.
- `reduce_elem_dt` gating matches when CDT3 reduces DT2T, given the
  `lectur.F` default `IDTMIN(3)=2` for `NODADT==0`.
- DT2T ordering in `resol.F`: sync/scatter happens after the element loop
  and before DT2T is consumed for the next cycle.

### Known gaps (documented, not yet implemented)

- **`NELTST`/`ITYPTST` are not updated** when the GPU reduces DT2T, so the
  "controlling element" printed in the listing is stale/wrong on GPU cycles
  (cosmetic; would require passing them into `gpu_shell_sync_scatter`).
- **`IDTMIN(3)` actions are not implemented on the GPU**: element deletion
  (`/DT/SHELL/DEL`, IDTMIN=2), stop (IDTMIN=1/5), constant-dt small-strain
  switch (IDTMIN=3) are silently skipped for offloaded groups.
- **`IDT1SH==1` (`/DT1/SHELL`) and AMS (`IDTMINS==2`) decks**: CPU `CDT3`
  still reduces DT2T with the mass/stiffness formula
  `DT = DTFAC1(3)*sqrt(0.5*MAS/STI)`; the GPU maps these to
  `compute_sti=1` with `reduce_elem_dt=.false.` → no element dt at all.
  Do not use those options with `-gpu` for now.
- `GBUF%DT` (`G_DT`/`DTEL`, used by `/ANIM/ELEM/DT`) is not filled on GPU.
- `FORINTC`'s skip condition checks `ICRACK3D==0 .AND. ACTIFXFEM==0` but
  `FORINTC_PREPARE_GPU`'s selection does not — a group with those active
  would be computed on **both** CPU and GPU (double forces). Keep the two
  conditions in sync.

### Debug aids — read these lines in the output first

1. `[GPU-GLOBAL] Created global handle: NUMNOD=...` — printed by the CUDA
   driver (C code). **If it is missing, the executable has no CUDA support**
   and (since the fix) the engine falls back to CPU with a warning.
2. `[GPU-DT-CFG] SU=.. NODADT=.. IDT1SH=.. IDTMINS=.. IDTMIN3=..
   compute_sti=.. reduce_elem_dt=..` — printed at init. If
   `reduce_elem_dt=F`, the offloaded shells will **not** constrain DT2T:
   check the flags (`/DT/NODA` sets NODADT, `/DT1/SHELL` sets IDT1SH,
   `/DT/NODA/AMS` sets IDTMINS=2). An explicit warning is printed in that
   case.
3. First 5 cycles: `[GPU] dt_elem_min = ... reducing dt2t from ...`;
   every 100 cycles `[GPU-DT] CYC=... dt_min=...`. If `dt_min` shows `0.0`
   the reduction never ran; if it shows `~1e30`/`~1e308` either no element
   passed the `OFF>0` filter or `reduce_elem_dt` is false.
4. `[GPU-CFG]` (first cycle) lists SU sizes; `fort.700` logs super-group
   splits.
