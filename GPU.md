# OpenRadioss GPU port — current state

This document describes the CUDA offload currently implemented in the
OpenRadioss **engine**: what is ported, how to build and run it, how it is
architected, and what its known limitations are.

For the detailed per-kernel workflow, the time-step verification against the
CPU Fortran code, and the debugging guide, see
[`engine/source/elements/shell/coque/GPU_WORKFLOW.md`](engine/source/elements/shell/coque/GPU_WORKFLOW.md).

---

## 1. Scope of the port

The offload covers the **internal force computation of 4-node shell elements
with material law 2 (Johnson-Cook / PLAS_JOHNS)**, i.e. the CPU call chain
`FORINTC → CFORC3 → (CCOOR3, CNVEC3, CDERI3, CDLEN3, CCOEF3, CDEFO3, CCURV3,
CSTRA3, CMAIN3/SIGEPS02C/M2CPLR, CHVIS3, CFINT3, CDT3, CUPDT3)`.

An element group is offloaded only if **all** of the following hold
(`FORINTC_PREPARE_GPU` selection, mirrored by the skip test in `FORINTC`):

| Criterion | Meaning |
|---|---|
| `ITY == 3` | 4-node shells (no triangles/SH3N as primary type) |
| `MLW == 2` | material law 2 |
| `NPT > 0` | through-thickness integration points (not global integration) |
| `JHBE < 11` | Belytschko-Tsay family (no QEPH=24, no BATOZ=11/12) |
| `IGTYP < 29` | no user properties |
| `IXFEM == 0`, `IFAILURE == 0` | no XFEM, no failure models |
| `NSECT == 0`, `NEXMAD == 0` | no sections, no MADYMO coupling |
| `NSLIPRING == 0`, `ISEATBELT == 0` | no seatbelt features |
| `ISUBSTACK == 0`, `ISENS_ENERGY == 0` | no stack property, no energy sensors |

Groups passing the filter are packed into **super-groups (SU)** — contiguous
merges of groups with identical `NPT`, `ISMSTR`, `JHBE`, `ITHK`, `IPLA` — so
that one CUDA kernel launch covers many Fortran groups. Groups that do not
pass stay on the normal CPU path; `FORINTC` skips exactly the offloaded ones.

Everything else in the engine (solids, contact, other materials/properties,
time integration, assembly, SPMD communication) remains on the CPU.

## 2. Building

The CUDA sources (`engine/source/elements/shell/coque/*.cu`) are compiled
**only with the NVIDIA HPC SDK toolchain**:

```bash
cd engine
./build_script.sh -arch=linux64_nvidia -mpi=ompi [-gpu-cc=cc80]
```

- `-gpu-cc=ccXX` sets the compute capability (default `cc80`); it defines the
  CMake variable `gpu_cc`, which is what enables compilation of the `.cu`
  files (as C++ with `nvc++ -cuda`) and defines the `WITH_CUDA` preprocessor
  macro for the Fortran side.
- `-openacc` additionally enables OpenACC offload flags for the Fortran
  compiler; it is independent of the shell CUDA port.
- The `.cu` files receive the same `-DMYREAL8/-DMYREAL4` precision flag as the
  Fortran code, so `Real` (see `real_type.h`) always matches `my_real`/`WP`.

**Any other toolchain** (gfortran, ifx, ifort, AOCC) silently drops the `.cu`
files; `shell_gpu_mod.F90` then provides empty stub implementations so the
executable still links. The module parameter `gpu_shell_available` is
`.false.` in that case and the engine refuses to enter the GPU code path at
runtime (see §6).

## 3. Running

The offload is activated with the engine command-line flag:

```bash
engine_linux64_nvidia -i model_0001.rad -gpu
```

Without `-gpu` the engine behaves exactly as a CPU build. At startup with
`-gpu` you should see, in this order:

```
 [GPU-GLOBAL] Created global handle: NUMNOD=..., d_raw_h2d=.. MB, d_raw_d2h=.. MB
 [GPU-DT-CFG] SU= 1  NODADT=.. IDT1SH=.. IDTMINS=.. IDTMIN3=..  compute_sti=..  reduce_elem_dt=..  dtfac=..
 [GPU-CFG] SU= 1  NUMELC=...  NUMNOD=...  ISMSTR=...
```

The `[GPU-GLOBAL]` banner comes from the CUDA driver code — if it is missing,
the binary has no CUDA support (and falls back to CPU with a warning).

## 4. Architecture

### Files

| File | Role |
|---|---|
| `engine/source/elements/shell/coque/shell_gpu_driver.cu` | device memory, transfers, kernel launches, min-dt reduction |
| `engine/source/elements/shell/coque/shell_geometry_kernel.cu` | Kernel 1 — geometry (CCOOR3+CNVEC3+CDERI3+CDLEN3) |
| `engine/source/elements/shell/coque/shell_strain_material_kernel.cu` | Kernel 2 — strains + Johnson-Cook material (CDEFO3+CCURV3+CSTRA3+SIGEPS02C/M2CPLR) |
| `engine/source/elements/shell/coque/shell_force_assembly_kernel.cu` | Kernel 3 — hourglass + internal forces + nodal assembly (CHVIS3+CFINT3+CUPDT3) |
| `engine/source/elements/shell/coque/shell_gpu_data.h` | `ShellGPUData` (per SU) / `ShellGPUGlobal` (shared) structs |
| `engine/source/elements/shell/coque/shell_gpu_mod.F90` | Fortran `bind(c)` interfaces + host mirror types + non-CUDA stubs |
| `engine/source/elements/shell/coque/shell_internal_forces.F90` | init (`FORINTC_PREPARE_GPU`), per-cycle launch/collect |
| `engine/source/engine/resol.F` | integration into the explicit cycle |

### Data layout

- **One global handle** (`ShellGPUGlobal`) holds the full-mesh node arrays on
  device: one H2D buffer `[9*NUMNOD] = X|V|VR` and one D2H buffer
  `[8*NUMNOD] = Fx|Fy|Fz|Mx|My|Mz|STIFN|STIFR`, plus a dedicated CUDA stream
  and an `upload_done` event. `NODES%X/V/VR` and the host D2H buffer are
  page-locked (`cudaHostRegister`) for truly asynchronous transfers.
- **One `ShellGPUData` per super-group** holds connectivity (0-based global
  node ids), element state (OFF, THK, strains, hourglass, EINT), material
  scalars, per-integration-point state (5 stress components, plastic strain,
  strain rate, back-stress, temperature), all intermediate arrays between
  kernels, and its own stream + `kernels_done` event. Node/force pointers are
  aliases into the global handle.
- Constant data (connectivity, material, initial state from `ELBUF_TAB`) is
  uploaded **once** at initialization; only `X/V/VR` go host→device and only
  forces/stiffness (+ 8-byte min-dt scalars) come back each cycle.

### Per-cycle sequence (in `resol.F`)

```
gpu_shell_launch_async            ! before contact & CPU element work
  1  global H2D of X/V/VR + zero force arrays + record upload_done
  2  per SU: wait upload_done, launch K1→K2→K3, record kernels_done
  3  global stream waits all kernels_done, enqueue single force D2H
  4  per SU: enqueue min-dt reduction kernel + 8-byte D2H  (element-dt mode)
... INTFOP8 (contact) + FORINTC + FORINT execute on the CPU meanwhile ...
gpu_shell_sync_scatter            ! after the CPU element loop
  1  synchronize global + SU streams
  2  scatter forces/moments into NODES%A/AR, STIFN/STIFR into NODES%stifn/stifr
  3  collect per-SU min element dt (+ host-side fallback, see below)
  4  DT2T = min(DT2T, dt_elem_min)
```

All GPU work is asynchronous and overlaps the CPU contact + element loops;
the sync at collection time normally returns immediately.

## 5. Time step

Two modes, chosen at initialization to mirror the CPU `CDT3` behavior:

| Engine configuration | CPU behavior | GPU behavior |
|---|---|---|
| `NODADT==0` (element dt, default; `IDTMIN(3)` is defaulted to 2 by `lectur.F`) | `CDT3`: `DT = DTFAC1(3)·ALDT/SSP`, reduces DT2T | K1 stores ALDT² (exact CDLEN3 formula); a reduction kernel computes `min(DTFAC1(3)·√ALDT²/SSP)` over active elements; an 8-byte D2H returns it; DT2T is reduced in `gpu_shell_sync_scatter` |
| `NODADT/=0` (`/DT/NODA*`, nodal dt) | element dt not used; STIFN drives the nodal dt | K3 computes CHVIS3/CHSTI3-style STI/STIR and scatters into STIFN/STIFR (verified against `chvis3.F`; assembly `ASSPAR4` accumulates, so Parith/ON is safe) |
| `NODADT==0` with `/DT1/SHELL` or `/DT/AMS` | `CDT3` mass/stiffness (or AMS) formula | **not implemented** — the engine detects this, warns, and disables the GPU offload (full CPU fallback) |

A **host-side fallback** guards the element-dt mode: if the collected GPU
reduction result is not a plausible dt, the host downloads ALDT² and
recomputes the minimum itself, so DT2T stays correct while the discrepancy is
reported (`[GPU-DT-HOST]` lines).

For the law-2 specifics: the CPU factor `VISCMX/√ALPE` equals 1
(`VISCMX=0` from the material → transformed to 1; `ALPE=1` in `ccoef3.F`),
and `SSP = PM(27,imat)` on both sides, so the GPU formula matches `CDT3`
exactly for the supported configurations.

## 6. Safety fallbacks (engine refuses to run the GPU path)

The engine automatically **disables the offload and falls back to the CPU
path, with a warning in the listing and stdout**, when:

1. the executable was built without CUDA (`gpu_shell_available = .false.`) —
   otherwise the no-op stubs would silently produce zero shell forces and a
   broken time step;
2. `NODADT==0` with `IDT1SH==1` (`/DT1/SHELL`) or `IDTMINS==2` (`/DT/AMS`) —
   element-dt formulas not implemented on GPU (see table above).

## 7. Known limitations

- Only law 2, 4-node BT shells (see §1 for the full filter).
- `NELTST`/`ITYPTST` (the "controlling element" shown in the listing) are not
  updated when the GPU reduces DT2T — the printed controlling element can be
  stale on GPU-controlled cycles.
- `IDTMIN(3)` actions — element deletion (`/DT/SHELL/DEL`), stop, constant-dt
  small-strain switch — are not applied to offloaded elements.
- `GBUF%DT` (`/ANIM/ELEM/DT` output) is not filled for offloaded elements.
- Element/IP state lives on the GPU during the run; it is only downloaded for
  output/restart via `shell_gpu_download_state` — animation/TH output of
  offloaded groups reflects the initial upload unless a download is added at
  output points.
- Energy balance (`PARTSAV`) for offloaded groups is accumulated only on
  print cycles (`ipri>0`) from `EINT` downloads.
- Super-group homogeneity does not include the material id: merging law-2
  groups with **different materials** into one SU would apply the first
  group's material to all of them (all elements of an SU currently share one
  parameter set). Multi-material law-2 models should be checked against the
  CPU reference.
- The `FORINTC` skip filter has two extra conditions (`ICRACK3D==0`,
  `ACTIFXFEM==0`) that the `FORINTC_PREPARE_GPU` selection lacks — keep both
  filters strictly in sync when editing either.
- Single-GPU per MPI rank; no GPU-aware MPI. SPMD works because each domain
  offloads only its own elements and DT2T/forces go through the normal
  exchanges.

## 8. Diagnostics

All GPU prints are prefixed and greppable; in order of usefulness:

| Print | Meaning |
|---|---|
| `[GPU-GLOBAL]` | CUDA driver alive; missing = no CUDA in the binary |
| `[GPU-DT-CFG]` | time-step flags seen at init, `compute_sti`, `reduce_elem_dt` |
| `[GPU-CFG]` | super-group sizes (`NUMELC=0` = nothing offloaded) |
| `[GPU-DT-RAW]` | first 3 cycles: raw per-SU reduction results (NaN detected explicitly) |
| `[GPU-DT-HOST]` | host-side fallback engaged: GPU vs host min-dt values |
| `[GPU] dt_elem_min` | first 5 cycles: DT2T actually reduced by the GPU element dt |
| `[GPU-DT]` | periodic: current min element dt and controlling element info |

The full decision table for interpreting `dt_min` values (huge/1e30/1.38e306/
0.0) is in `GPU_WORKFLOW.md` §Debug aids.

## 9. Performance notes (from nsys profiling, 28k-cycle run)

- Kernel 2 (strain + Johnson-Cook radial return) dominates GPU compute
  (~71%); register pressure limits occupancy — main optimization target.
- Host memory pinning of the node/force buffers removed the dominant
  `cudaMemcpyAsync` staging cost and enables real CPU/GPU overlap.
- Per cycle and per SU: 1 H2D + 3 kernel launches + 1 D2D + 1–2 D2H + 1 sync.
- The min-dt GPU reduction replaces a per-cycle `NUMELC`-sized D2H + host loop
  with one kernel + an 8-byte D2H.
- Long-term direction: keep node data GPU-resident (GPU time integration) to
  eliminate the per-cycle H2D/D2H entirely.

## 10. Change log of the port's integration fixes (2026-07)

| Commit | Fix |
|---|---|
| `076330d` | `gpu_shell_launch_async` was nested inside `IF(NINTER/=0)` — decks without contact interfaces never launched the pipeline (zero forces, DT2T zeroed); launch moved out, DT2T reduction hardened (`dt_gpu_min > 0`, `dt_min_result` init 1e30) |
| `24ec59e` | `-gpu` on a non-CUDA build silently ran no-op stubs; added the real `gpu_shell_available` guard + CPU fallback + `[GPU-DT-CFG]` diagnostics |
| `4c1d2de` | `/DT1/SHELL` and `/DT/AMS` element-dt formulas are not implemented on GPU; engine now warns and falls back to CPU instead of running with an unconstrained time step |
| `d877b70` | raw per-SU min-dt diagnostics (`[GPU-DT-RAW]`, NaN detection) + host-side element-dt fallback (`[GPU-DT-HOST]`) + `numelc=0` guard in the energy download |
