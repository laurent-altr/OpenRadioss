# GPU Port Investigation: `sigeps36c` (MAT036 Shell Material Law)

## Executive Summary

This document investigates what is required to port `sigeps36c.F` to run on
GPU via **OpenMP target offloading** (`!$OMP TARGET`).  The investigation
covers the full call stack from `resol.F` down to the interpolation routines,
identifies all GPU-incompatible patterns, and proposes concrete changes for
each one.

The key conclusion is that a GPU port is **feasible** with three categories of
change:

1. Replace the stateful sequential-search interpolation (`VINTER`/`VINTER2`)
   with a stateless O(1) regular-spacing lookup (`VINTER_REG`) — already
   agreed as a prerequisite.
2. Convert the implicit-scheme "gather" loops to predicated parallel loops.
3. Restructure the offload entry point so the `!$OMP TARGET` region is
   launched from a context that compilers accept (single thread, outside or
   compatible with the enclosing `!$OMP PARALLEL`).

---

## 1. Call Stack and Current Parallelism

```
resol.F:1607  !$OMP PARALLEL   ← CPU thread parallel region
  └─ RESOL_INIT()              ← SMP task dispatch (one task = one element group)
       └─ czforc3.F            ← shell force/kinematics kernel
            └─ cmain3.F:320    ← CALL MULAWC(...)
                 └─ mulawc.F90 ← material law dispatcher (switch on material type)
                      └─ sigeps36c.F  ← MAT036 constitutive update
                           ├─ VINTER / VINTER2  (curve interpolation)
                           └─ VINTER2           (optional Young's modulus scaling)
```

Current CPU parallelism is purely **OpenMP thread-parallel** at the
`resol.F` level.  Each CPU thread owns a disjoint subset of element groups;
within one group, `sigeps36c` processes `NEL ≤ MVSIZ = 129` elements in SIMD
loops decorated with `#include "vectorize.inc"`.

There is **no existing GPU code** anywhere in the engine (no `!$OMP TARGET`,
no `!$ACC`, no `.cu` files).

---

## 2. GPU-Incompatible Patterns in `sigeps36c.F`

### 2.1 `VINTER` — Sequential Walk Interpolation (blocking issue)

File: `engine/source/tools/curve/vinter.F:72`

```fortran
! Simplified structure of VINTER:
J = 0
100 CONTINUE
  J = J+1
  ICONT = 0
  DO I=1,NEL
    J1 = IPOS(I)+IAD(I)+1
    IF (J<=ILEN(I)-1 .AND. X(I)>TF(1,J1)) THEN
      IPOS(I) = IPOS(I)+1   ! advance position for this element
      ICONT = 1
    ENDIF
  ENDDO
IF (ICONT==1) GOTO 100      ! loop until no element advances
```

**Problems for GPU:**
- Loop-carried dependency on `IPOS(I)` — each pass potentially modifies the
  next pass's input; this prevents fusing into a single parallel kernel.
- Data-dependent termination via `ICONT` — requires a global reduction before
  knowing whether to continue.  On a GPU this maps to either an atomic or a
  synchronisation barrier per outer iteration.
- `IPOS` is **persisted** between timesteps in `VARTMP(I, 2+J1)`.  Because
  PLA is monotonically non-decreasing (plastic strain), VINTER's walk only
  ever moves forward, giving O(1) amortised cost on CPU.  On GPU the state
  can't be cheaply maintained per-element across timesteps unless VARTMP is
  kept resident on device — but that is extra complexity.

**Resolution:** Replace with `VINTER_REG` (see Section 4).

### 2.2 `VINTER2` — Bidirectional Walk (same class of problem)

File: `engine/source/tools/curve/vinter.F:143`

Same sequential walk but also allows IPOS to decrease.  Used for:
- Pressure-dependent yield scale (`PFUN > 0`) — `PSCALE` can increase or
  decrease.
- Variable Young's modulus (`OPTE == 1`).

`VINTER2` also has the partial-element gather pattern: only elements where
`PLA(I) > ZERO` are packed into `INDEX_PLA` before the call (lines 214–226).
Both the gather and the bidirectional walk are problematic.

**Resolution:** `VINTER_REG` handles bidirectional lookup without state
because the index is computed directly from the coordinate value.

### 2.3 Gather-Then-Compute Pattern for Plastic Elements

The implicit scheme (`IPLAS == 1`, lines 524–592; `IPLAS == 2`, lines 610–660;
`VP == 1`, lines 852–920) all follow the same pattern:

```fortran
NINDX = 0
DO I = 1, NEL
  IF (SVM2(I) > YLD(I)*YLD(I) .AND. OFF(I) == ONE) THEN
    NINDX = NINDX + 1
    INDEX(NINDX) = I   ! sequential, atomic on GPU → serialises
  ENDIF
ENDDO

IF (NINDX > 0) THEN
  DO J = 1, NINDX      ! indirect access INDEX(J) → gather → non-coalesced
    I = INDEX(J)
    ...
```

**Problems for GPU:**
- `NINDX = NINDX+1` is a sequential scalar accumulation — maps to an
  `atomicAdd` on GPU but the resulting INDEX array is unordered, leading to
  serialised writes.
- The `DO J=1,NINDX … I=INDEX(J)` loop has indirect/random access to
  stress arrays via `INDEX(J)` — scattered memory access, poor coalescing.

**Resolution:** Drop the gather entirely; use a predicated loop (Section 5.1).

### 2.4 Scalar Module-Level State (`IMPL_S`, `IKT` from `impl1_c.inc`)

Lines 953–971 read `IMPL_S` and `IKT` from a common block
(`#include "impl1_c.inc"`).  Common blocks are not natively mappable via
`!$OMP TARGET MAP`.

**Resolution:** Pass `IMPL_S` and `IKT` as explicit scalar arguments to the
GPU subroutine (they are scalars, cheaply copied).

### 2.5 `!$OMP TARGET` Inside `!$OMP PARALLEL`

The existing `!$OMP PARALLEL` at `resol.F:1607` encloses the entire
element-force computation.  OpenMP 5.0 permits a `TARGET` construct to appear
inside a `PARALLEL` region (the thread that encounters `TARGET` becomes the
initiating thread of the target task), but:
- Not all compilers implement this correctly (see Section 6 for compiler notes).
- With `!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO`, the device-side
  parallelism is independent of the host parallel region.

The recommended approach (Section 3) sidesteps this entirely.

---

## 3. Recommended Call-Stack Restructuring

### Option A — Separate GPU Material Pass (Recommended)

Split the per-timestep computation into two sub-steps in `resol.F`:

```
Step 1 (outside !$OMP PARALLEL):
  → For each element group that is MAT36 AND gpu_available:
      CALL SIGEPS36C_GPU(...)    ← launches !$OMP TARGET kernel
      (processes entire group in parallel on GPU)

Step 2 (!$OMP PARALLEL as today):
  → RESOL_INIT() as today but skip MAT36 groups already processed
```

**Advantages:**
- Clean separation; no nested offload issue.
- GPU kernel can process a much larger batch (the entire set of MAT36 elements,
  not just one MVSIZ chunk at a time).
- Allows pipelining: GPU computes material law while CPU handles kinematics
  of other element types.

**Disadvantages:**
- Requires changes in `resol.F` (or in the SMP task dispatch) to mark
  processed groups.
- Needs a "GPU MAT36 element set" list assembled before the time loop.

### Option B — `!$OMP MASTER` Guard Inside PARALLEL

Keep the existing parallel region; add an `!$OMP MASTER` guard around the
`sigeps36c_gpu` call inside `mulawc.F90`:

```fortran
!$OMP MASTER
  CALL SIGEPS36C_GPU(...)   ! launches TARGET kernel from one thread
!$OMP END MASTER
!$OMP BARRIER
```

**Advantages:** No restructuring of `resol.F`.

**Disadvantages:** While the GPU runs, all other CPU threads wait at the
barrier — wastes CPU cores.  Load balancing is poor if MAT36 work is large.

### Option C — `!$OMP TARGET NOWAIT` with `!$OMP TASKWAIT`

Modern OpenMP supports asynchronous offload:

```fortran
!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO NOWAIT
  ...
!$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO

! CPU threads continue with other work
...

!$OMP TASKWAIT  ! synchronise before using GPU results
```

This is the most efficient option but requires careful dependency tracking.
Not recommended for the initial implementation.

---

## 4. New Interpolation Function: `VINTER_REG`

Assumes curves have been resampled to regular spacing (preprocessing step,
already agreed).  The resampled data structure for each curve `k` consists of:
- `TFR_Y(k, 1:NXR)` — resampled Y values
- `TFR_DYDX(k, 1:NXR)` — precomputed slopes (optional, alternatively computed
  on the fly)
- `TFR_XMIN(k)` — X coordinate of first point
- `TFR_DX(k)` — regular spacing

```fortran
! Proposed signature — to be placed in a new file
! engine/source/tools/curve/vinter_reg.F90

SUBROUTINE VINTER_REG(TFR_Y, NXR, XMIN, DX_INV, NEL, X, DYDX, Y)
  !$OMP DECLARE TARGET
  INTEGER,  INTENT(IN)  :: NEL, NXR
  my_real,  INTENT(IN)  :: XMIN, DX_INV   ! DX_INV = 1/DX
  my_real,  INTENT(IN)  :: TFR_Y(NXR)     ! resampled Y-values (base already offset)
  my_real,  INTENT(IN)  :: X(NEL)
  my_real,  INTENT(OUT) :: DYDX(NEL), Y(NEL)
  INTEGER :: I, IDX
  my_real :: T, DX
  DX = ONE / DX_INV
  !$OMP SIMD
  DO I = 1, NEL
    IDX  = INT((X(I) - XMIN) * DX_INV)
    IDX  = MAX(0, MIN(IDX, NXR - 2))   ! clamp to valid range
    T    = (X(I) - (XMIN + IDX*DX)) * DX_INV
    DYDX(I) = (TFR_Y(IDX+2) - TFR_Y(IDX+1)) * DX_INV
    Y(I)    = TFR_Y(IDX+1) + T * (TFR_Y(IDX+2) - TFR_Y(IDX+1))
  END DO
END SUBROUTINE VINTER_REG
```

Key properties:
- **O(1) per element** — no loop, no GOTO, no data-dependent termination.
- **No persistent state** — `IPOS`/`VARTMP` writes are eliminated entirely.
- **Fully SIMD/warp-friendly** — no branch divergence except the clamp.
- Decorated with `!$OMP DECLARE TARGET` so it can be called from within a
  TARGET region.

The VINTER2 replacement (`VINTER2_REG`) is identical — regular-spacing lookup
is inherently bidirectional (no walk direction concept).

---

## 5. Restructuring `sigeps36c` for GPU

The GPU variant will be a new file:
`engine/source/materials/mat/mat036/sigeps36c_gpu.F90`

### 5.1 Replace Gather Loops with Predicated Loops

**Before (CPU):**
```fortran
NINDX = 0
DO I = 1, NEL
  IF (SVM2(I) > YLD(I)*YLD(I) .AND. OFF(I) == ONE) THEN
    NINDX = NINDX + 1
    INDEX(NINDX) = I
  ENDIF
ENDDO
DO J = 1, NINDX
  I = INDEX(J)
  DPLA_J(I) = (SVM-YLD(I))/(G3(I)+H(I))
  ...
ENDDO
```

**After (GPU):**
```fortran
!$OMP SIMD   ! (inside the TARGET region's parallel loop)
DO I = 1, NEL
  IF (SVM2(I) > YLD(I)*YLD(I) .AND. OFF(I) == ONE) THEN
    DPLA_J(I) = (SQRT(SVM2(I)) - YLD(I)) / (G3(I) + H(I))
    ...
  ELSE
    DPLA_J(I) = ZERO
    DPLA_I(I) = ZERO
  ENDIF
ENDDO
```

On a GPU, predicated execution (masked SIMD) is far cheaper than an atomicAdd
+ indirect array access, and the branch coherence is high in practice (most
elements in a shell model either all yield or none do within a small region).

### 5.2 Outer Kernel Structure

```fortran
SUBROUTINE SIGEPS36C_GPU(NEL, ...)
  !$OMP DECLARE TARGET
  ...
  !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO &
  !$OMP&   MAP(TO: TF_REG, UPARAM, IPM, NPF) &
  !$OMP&   MAP(TOFROM: PLA, UVAR, VARTMP, OFF, THK, YLD, &
  !$OMP&             SIGOXX, SIGOYY, ..., SIGNXX, SIGNYY, ...)
  DO I = 1, NEL
    ! Inline all per-element computations:
    !   - elastic predictor
    !   - strain-rate lookup (VINTER_REG)
    !   - yield surface evaluation
    !   - plastic return (3 Newton iterations, inlined)
    !   - kinematic hardening update
    !   - failure check
  ENDDO
  !$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO
END SUBROUTINE
```

**Note on inlining:** The Newton iteration loop (`DO N=1,NITER`) is only 3
iterations with ~15 arithmetic ops each per element.  It should be inlined
within the outer `DO I=1,NEL` loop to avoid kernel re-launch overhead.

### 5.3 VARTMP Changes

With `VINTER_REG`, the following writes are **removed**:
```fortran
VARTMP(I, 2+J1) = IPOS1(I)    ! ← no longer needed
VARTMP(I, 2+J2) = IPOS2(I)    ! ← no longer needed
VARTMP(I, 2)    = IPOSP(I)    ! ← no longer needed (pressure curve)
VARTMP(I, 1)    = IPOSPE(I)   ! ← no longer needed (E-modulus curve)
```

`VARTMP` is then only used for `UVAR(I,2)` (plastic strain rate in `VP=1`
mode), which is a simple scalar per element.

---

## 6. Data Mapping Strategy

For efficient GPU execution the goal is to **minimise host↔device transfers**
per timestep:

| Array           | Size               | Direction | Residency |
|-----------------|--------------------|-----------|-----------|
| `TF_REG`        | #curves × NXR      | TO        | Permanent (mapped once at startup) |
| `UPARAM`        | NUPARAM            | TO        | Permanent |
| `IPM`           | NPROPMI × NUMMAT   | TO        | Permanent |
| `PLA`           | NEL                | TOFROM    | Per timestep |
| `UVAR`          | NEL × NUVAR        | TOFROM    | Per timestep |
| `OFF`           | NEL                | TOFROM    | Per timestep (element deletion) |
| `THK`           | NEL                | TOFROM    | Per timestep |
| All stress arrays| NEL × 5           | TOFROM    | Per timestep |
| `EPSD`, `DEPS*` | NEL × 5            | TO        | Per timestep |
| `SIGOXX` etc.   | NEL × 5            | TO        | Per timestep |

The total per-timestep transfer for a batch of `NEL` elements (e.g. 128) is
roughly `NEL × 30 × sizeof(my_real)` ≈ 30 KB for `NEL=128` in double
precision.  This is small enough that transfers will not dominate unless the
GPU kernel itself is extremely fast.

For large models where MAT36 elements number in the millions, the
**permanent resident** strategy for `TF_REG`/`UPARAM`/`IPM` becomes
critical to avoid re-uploading static data every timestep.  This suggests
using `!$OMP TARGET ENTER DATA` at startup.

---

## 7. Compiler Considerations

| Compiler       | `!$OMP TARGET` in `!$OMP PARALLEL` | `DECLARE TARGET` routines | Status |
|----------------|------------------------------------|---------------------------|--------|
| nvfortran ≥ 22 | Supported (NVIDIA OpenMP 5.0)     | Yes                       | Recommended for NVIDIA GPUs |
| ifx / ifort    | Supported (Intel OpenMP 5.1)      | Yes                       | For Intel Arc/Xe |
| gfortran ≥ 13  | Partially (offload to AMD/NVIDIA) | Yes (experimental)        | Build with `-foffload=` |
| flang-new ≥ 18 | In progress                       | Partial                   | Not recommended yet |

**Recommended target for initial prototype:** `nvfortran` with NVIDIA GPU,
using `-mp=gpu -gpu=cc80` (Ampere architecture).

Build system change required: add GPU-enabled build target in
`CMakeLists.txt`, e.g.:
```cmake
option(OPENRADIOSS_GPU "Enable OpenMP GPU offload" OFF)
if(OPENRADIOSS_GPU)
  target_compile_options(engine PRIVATE -mp=gpu -gpu=cc80)
  target_compile_definitions(engine PRIVATE CPP_GPU=1)
endif()
```

---

## 8. Files to Create / Modify

| File | Action | Description |
|------|--------|-------------|
| `engine/source/tools/curve/vinter_reg.F90` | **Create** | O(1) regular-spacing interpolation, `!$OMP DECLARE TARGET` |
| `engine/source/materials/mat/mat036/sigeps36c_gpu.F90` | **Create** | GPU variant of sigeps36c; replaces VINTER with VINTER_REG, replaces gather loops with predicated loops, wraps element loop in `!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO` |
| `engine/source/materials/mat_share/mulawc.F90` | **Modify** | Add branch: if `GPU_AVAIL .AND. mat_type==36`, call `sigeps36c_gpu` instead of `sigeps36c` |
| `engine/source/engine/resol.F` | **Modify** (Option A only) | Move MAT36 material computation before `!$OMP PARALLEL`; or add `!$OMP MASTER` guard (Option B) |
| `engine/CMakeLists.txt` | **Modify** | Add `OPENRADIOSS_GPU` build option and GPU-enabled compiler flags |
| A new preprocessing module (name TBD) | **Create** | Resample TF curves to regular spacing at model initialization; store in a new array alongside TF |

---

## 9. Open Questions

1. **Regular-spacing resolution**: How many resampled points per curve?
   A spacing that gives < 0.1 % interpolation error for typical MAT36
   hardening curves needs to be determined.  Suggested: use the original
   curve's range divided by 4× the original point count.

2. **VP=1 mode (plastic strain rate dependency)**: The `UVAR(I,2)` array
   stores the filtered plastic strain rate.  This is an `INTENT(INOUT)`
   quantity that must be mapped `TOFROM`.  Check whether this creates a
   device→host transfer dependency that conflicts with later CPU computations
   in the same timestep.

3. **`SIGNOR(MVSIZ, 5)` output for implicit solver**: Used only when
   `IMPL_S > 0`.  This is a fixed-size scratch array (`MVSIZ=129`).  For the
   GPU variant it can be declared as a private per-element scalar set inside
   the parallel loop.

4. **Failure modes**: Element deletion (`OFF(I) = FOUR_OVER_5`) writes to
   `OFF` on the device.  The deleted element list must be synchronised back
   to host before the next force-assembly step that uses `OFF`.

5. **Thread safety of `mulawc` dispatch**: `mulawc.F90` is called inside
   `!$OMP PARALLEL`; if Option B is chosen, the `!$OMP MASTER` guard
   serialises MAT36 groups.  Ensure no race on shared counters or timers
   (`TIMERS` array).

---

## 10. Estimated Scope of Changes

| Category | Effort |
|----------|--------|
| `vinter_reg.F90` (new interpolation) | ~50 lines of Fortran |
| `sigeps36c_gpu.F90` (GPU variant) | ~400 lines (modified copy of sigeps36c.F) |
| `mulawc.F90` dispatch change | ~20 lines |
| `resol.F` restructuring (Option A) | ~50 lines |
| CMake / build system | ~30 lines |
| Curve resampling preprocessing | ~100 lines |
| **Total new/modified lines** | **~650** |

No changes to the Starter are required if resampling is performed lazily on
first GPU call in the engine (per the agreed approach).
