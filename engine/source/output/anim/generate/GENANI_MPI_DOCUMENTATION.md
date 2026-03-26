# GENANI — Animation Output: MPI Communication Pattern

## Overview

`GENANI` (`engine/source/output/anim/generate/genani.F`) is the main subroutine
responsible for writing one animation frame to the binary `.anim` file.  It is
called from two places:

| Caller | File | Context |
|--------|------|---------|
| `SORTIE_MAIN` | `engine/source/output/sortie_main.F:628` | Regular time-history output |
| `IMP_BUCK`    | `engine/source/implicit/imp_buck.F:857`  | Implicit buckling eigenmode output |

---

## High-level architecture (parallel run)

In an MPI run (`NSPMD > 1`) the domain is split across `NSPMD` processes.
**Rank 0 (ISPMD == 0) is the sole writer**: it opens the animation file, writes
every binary record and closes it.  All other ranks are data providers.

```
Rank 1..N-1                    Rank 0
-----------                    ------
  compute local                  open animation file
  field values                   write file header
       |                              |
       | MPI_SEND (point-to-point)    | MPI_RECV from each rank
       |------------------------------>|
       |                              | assemble global array
       |                              | WRITE_R_C / WRITE_I_C
       |  (repeat for every output    |
       |   variable)                  |
       |                              |
                                  close animation file
```

This is a **blocking, synchronous** operation: every MPI process participates
in every gather call and does not return from `GENANI` until the complete frame
has been written to disk.

---

## Phase-by-phase description

### Phase 1 — Global dimension synchronisation (pre-write)

Before writing anything, the processes need to agree on global sizes.

| Code location | Routine called | Operation |
|---------------|----------------|-----------|
| `genani.F:1054` | `SPMD_GLOB_IMAX9(IDMAX,1)` | `MPI_Allreduce MAX` — global max node ID for PLY animation |
| `genani.F:1078` | `SPMD_FVB_ADIM(...)` | Point-to-point gather of FV-bag dimensions to rank 0 |
| `genani.F:1093` | `SPMD_CRK_IDMAX(IDMAX,ITAB)` | Reduce max node ID for XFEM crack nodes |
| `genani.F:1094` | `SPMD_MAX_XFE_I(IDMAX)` | `MPI_Allreduce MAX` — max XFEM extended node ID |
| `genani.F:1268` | `SPMD_MAX_XFE_I(IDCRK)` | `MPI_Allreduce MAX` — max crack-element node ID |

### Phase 2 — File header (rank 0 only)

Rank 0 writes the binary frame header:

```fortran
IF (ISPMD==0) THEN
  CALL WRITE_I_C(MAGIC=21548, 1)   ! 0x542b — file signature
  CALL WRITE_R_C(TT, 1)            ! simulation time
  CALL ANI_TXT('Time=', 5)
  CALL WRITE_I_C(NUMNODG, 1)       ! global node count
  ...
ENDIF
```

Workers are idle during this phase.

### Phase 3 — Node coordinates (XYZ gather)

```fortran
! genani.F:1233
CALL XYZNOD(X, X_TEMP, NODGLOB, WEIGHT)
```

`XYZNOD` internally calls `SPMD_GATHERF` (or equivalent) for each coordinate
component.  Workers send their local nodal coordinates; rank 0 assembles and
writes the global coordinate array.

Immediately after, the surface normal vectors at partition boundaries are
**exchanged** (not gathered to rank 0):

```fortran
! genani.F:1335-1336
LENR = IAD_ELEM(1,NSPMD+1) - IAD_ELEM(1,1)
CALL SPMD_EXCH_N(XNORM, IAD_ELEM, FR_ELEM, LENR)
```

`SPMD_EXCH_N` is the **only non-blocking routine** in the animation path — it
uses `MPI_IRECV` / `MPI_ISEND` followed by `MPI_WAIT` for each neighbour pair,
and **accumulates** (sums) the shared normals.

### Phase 4 — Element connectivity and part sorting

After the coordinate phase rank 0 writes element connectivity (node lists,
element IDs, material IDs).  Workers provide no data here; rank 0 operates on
data that was gathered at mesh-partitioning time and stored in global arrays.

### Phase 5 — Nodal/element results (field gather loop)

This is the most repeated pattern.  For every requested output variable
(velocities, displacements, stresses, strains, …) the pattern is:

```fortran
! genani.F:3179-3194  (nodal scalar example)
IF (NSPMD == 1) THEN
  CALL WRITE_R_C(WA4, NUMNOD)          ! serial: direct write
ELSE
  IF (ISPMD==0) THEN
    CALL SPMD_GATHERF(WA4, WEIGHT, NODGLOB, NUMNODG)
  ELSE
    CALL SPMD_GATHERF(WA4, WEIGHT, NODGLOB, 1)
  END IF
END IF
```

Note that both rank 0 and workers call the **same routine** with a different
`NUM` argument so they take different branches internally.

### Phase 6 — Nodal mass

```fortran
! genani.F:4105-4116
CALL SPMD_GATHERF(WA4, WEIGHT, NODGLOB, BUF)
```

Same pattern as Phase 5.

### Phase 7 — Node numbering (integer gather)

```fortran
! genani.F:4166-4172
CALL SPMD_GATHERITAB(ITAB, WEIGHT, NODGLOB, BUF)
```

Uses the integer variant `SPMD_GATHERITAB` (message tags 7018/7019).

### Phase 8 — Flexible-volume-bag (FVB/airbag) animation

FV bags have a dedicated set of MPI routines because each bag is owned by a
single **primary process** (`PMAIN`), not necessarily rank 0.  The pattern:

```
  PMAIN process → MPI_SEND velocity/node data → Rank 0 → WRITE_R_C
```

Routines: `SPMD_FVB_ANOD`, `SPMD_FVB_AVEC`, `SPMD_FVB_ATR`, `SPMD_FVB_AOFF`,
`SPMD_FVB_APAR`, `SPMD_FVB_ATIT`, `SPMD_FVB_ASUB1/2`, `SPMD_FVB_AELF`,
`SPMD_FVB_AMON`.

---

## SPMD_GATHERF — detailed walk-through

**File:** `engine/source/mpi/anim/spmd_gatherf.F`

```
Arguments:
  V(*)        - local array of floats (size NUMNOD)
  WEIGHT(*)   - mask: 1 = "this node is mine, include it"
  NODGLOB(*)  - global node index for each local node
  NUM         - size of global output array (NUMNODG on rank 0, 1 elsewhere)
```

**Worker path (ISPMD != 0):**

1. Pack `IBUF(SIZ)` ← global indices of owned nodes.
2. Pack `BUFSR(SIZ)` ← corresponding float values.
3. `MPI_SEND(IBUF,  SIZ, MPI_INTEGER, rank0, tag=7015)`
4. `MPI_SEND(BUFSR, SIZ, MPI_REAL,    rank0, tag=7014)`

**Rank-0 path (ISPMD == 0):**

1. Fill own entries: `XGLOB(NODGLOB(I)) = V(I)` for `WEIGHT(I)==1`.
2. For each remote rank `I = 2..NSPMD`:
   - `MPI_PROBE` to determine incoming message size.
   - `MPI_RECV` indices (tag 7015), then values (tag 7014).
   - Scatter: `XGLOB(IBUF(K)) = BUFSR(K)`.
3. `CALL WRITE_R_C(XGLOB, NUMNODG)` — write assembled global array to file.

**Key point:** receives are **sequential** (rank 2, then 3, …, then N).
There is no overlap between receiving from one worker and writing to disk.

---

## SPMD_GATHERITAB — integer variant

**File:** `engine/source/mpi/anim/spmd_gatheritab.F`

Same algorithm as `SPMD_GATHERF` but for `INTEGER` data.  Workers send a
packed 2D buffer `BUFSR(2, SIZ)` containing `(NODGLOB, ITAB)` pairs in a
single message (tag 7018).  Rank 0 unpacks and calls `WRITE_I_C`.

---

## SPMD_EXCH_N — non-blocking normal exchange

**File:** `engine/source/mpi/generic/spmd_exch_n.F`

Unlike the gather routines, `SPMD_EXCH_N` is **all-to-all** and is used
*before* any file write, to ensure shared boundary normals are consistent.

```
1. Post all MPI_IRECV   (non-blocking)
2. Pack SBUF with local normals for each neighbour partition
3. Post all MPI_ISEND   (non-blocking)
4. MPI_WAIT each IRECV, accumulate: XNORM += received normal
5. MPI_WAIT each ISEND
```

Result: each process's `XNORM` array now holds correctly summed normals at
partition boundaries.

---

## MPI message tag summary

| Tag | Direction | Routine | Data |
|-----|-----------|---------|------|
| 7000 | all↔all | `SPMD_EXCH_N` | surface normals (REAL) |
| 7014 | worker→rank0 | `SPMD_GATHERF` | float values |
| 7015 | worker→rank0 | `SPMD_GATHERF` | global node indices |
| 7018 | worker→rank0 | `SPMD_GATHERITAB` | (index, value) integer pairs |
| 7048 | PMAIN→rank0 | `SPMD_FVB_*` | FV-bag dimensions |
| 7049 | PMAIN→rank0 | `SPMD_FVB_*` | FV-bag field values |

---

## Synchronisation cost and blocking behaviour

```
Time →

Rank 0:  [compute]──[recv2]──[recv3]──[recvN]──[write]──[recv2]──...
Rank 1:  [compute]──[send]────────────────────────────────[send]──...
Rank 2:  [compute]──[send]────────────────────────────────[send]──...
...
Rank N:  [compute]──[send]────────────────────────────────[send]──...
                    ←──────── GENANI duration ────────────►
```

All processes are **blocked** inside `GENANI` for the full duration of the
animation write.  Workers finish their sends quickly and then sit idle waiting
for rank 0 to process all gathers before returning.  The computational
timestep cannot advance until `GENANI` returns on every rank.

---

## Implications for asynchronous animation writing

The goal is to overlap animation I/O with the next computational timestep.
The current architecture presents the following obstacles:

### 1. Rank 0 is both a compute rank and the sole writer

Every gather requires rank 0 to receive, assemble and write, then move on to
the next variable.  An async design would need either:

- **Dedicated I/O rank**: assign one rank (e.g. rank `NSPMD`) exclusively to
  animation writing.  Compute ranks gather data to this I/O rank instead of
  rank 0.
- **Thread-level async on rank 0**: keep rank 0 as writer but launch file I/O
  in a separate thread while MPI communication for the next frame can proceed.

### 2. SPMD_GATHERF is sequential across workers

Rank 0 receives from workers one by one (rank 2, 3, …, N).  Each receive is
blocking (`MPI_RECV`).  Replacing this with `MPI_Irecv` for all workers
simultaneously before starting to process results would reduce rank-0 wait
time even within the current synchronous model.

### 3. Data must be snapshotted before compute resumes

If compute ranks advance to the next timestep, the arrays (`V`, `X`, `WA4`,
…) passed to `GENANI` will be overwritten.  Any async scheme requires either:

- **Double-buffering**: maintain a copy of all animation-relevant arrays
  at the time the frame is triggered, so compute can proceed on the live arrays.
- **MPI-level message buffering**: workers send all their data to the I/O rank
  up-front in a single phase, then return to computation.  The I/O rank
  assembles and writes without further involvement from compute ranks.

### 4. Gather calls are interleaved with WRITE calls

Currently each `SPMD_GATHERF` call both gathers *and* writes one variable.
For an async design the gather (data movement) and the write (disk I/O) would
need to be separated so that all gathers complete first (freeing compute ranks)
and the I/O rank writes independently afterwards.

### Suggested async architecture sketch

```
Phase A — data snapshot (all ranks participate, short)
  For each output variable:
    Workers: MPI_Send local data → I/O rank buffer
    I/O rank: MPI_Recv into pre-allocated frame buffer (no file writes yet)

Phase B — computation resumes on compute ranks (overlapped with Phase C)

Phase C — file write (I/O rank only)
  I/O rank assembles global arrays from frame buffer
  I/O rank writes binary animation frame to disk
```

The critical path saving is: compute ranks spend time in Phase A (network
transfer, already fast), then immediately return to Phase B.  Phase C runs
concurrently on the dedicated I/O rank.

The main cost of this approach is the **memory** required on the I/O rank to
hold the entire frame buffer while it is writing, plus the engineering effort
to separate the gather and write steps currently fused inside `SPMD_GATHERF`,
`SPMD_GATHERITAB`, and the FVB routines.
