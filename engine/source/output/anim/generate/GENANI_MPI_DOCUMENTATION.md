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

---

## Concrete async design using the existing spmd_mod infrastructure

This section describes a concrete implementation plan using the wrappers
already available in `engine/source/mpi/spmd_mod.F90`.

### Available non-blocking primitives

`spmd_mod` re-exports the following relevant wrappers (all accept an optional
`comm` argument defaulting to `SPMD_COMM_WORLD`):

| Wrapper | Underlying MPI call | Signature |
|---------|---------------------|-----------|
| `spmd_isend` | `MPI_Isend` | `(buf, buf_count, dest, tag, request [,comm])` |
| `spmd_irecv` | `MPI_Irecv` | `(buf, buf_count, source, tag, request [,comm])` |
| `spmd_wait`  | `MPI_Wait`  | `(request [,status])` |
| `spmd_waitall` | `MPI_Waitall` | `(count, requests [,statuses])` |
| `spmd_waitany` | `MPI_Waitany` | `(count, requests, index [,status])` |
| `spmd_barrier` | `MPI_Barrier` | `([comm])` |

All are overloaded for `real`, `integer`, `double precision` arrays and scalars.

### Rank numbering with a dedicated I/O rank

The current code runs `NSPMD` compute processes (ISPMD 0 … NSPMD-1).
`IT_SPMD(I)` is the MPI rank of the I-th SPMD process (1-based index).
`IT_SPMD(1)` is the MPI rank of ISPMD==0 (master).

With a dedicated I/O rank the job is launched with **NSPMD+1** MPI processes.
The last process (`MPI rank = NSPMD`) becomes the I/O rank; it is not part
of the domain decomposition and has no finite-element arrays.

```
NSPMD  = N     (compute ranks, ISPMD 0..N-1, MPI ranks IT_SPMD(1)..IT_SPMD(N))
IO_RANK = N    (MPI rank of the I/O process, constant known to all ranks)
```

A new integer parameter `IO_RANK = NSPMD` can be stored in a module so all
animation routines can address it without hard-coding.

### Protocol: frame lifecycle

Three control messages (small integer tags distinct from the data tags) govern
the frame lifecycle.  Suggested tag constants:

```fortran
integer, parameter :: TAG_ANIM_START  = 9001  ! rank 0 → I/O rank: "frame coming"
integer, parameter :: TAG_ANIM_DONE   = 9002  ! I/O rank → rank 0: "write complete"
integer, parameter :: TAG_ANIM_STOP   = 9003  ! rank 0 → I/O rank: "simulation ended"
```

The `TAG_ANIM_START` message carries a single integer: the total number of
data messages the I/O rank should expect in this frame (so it knows when it
has received everything).

### Compute-rank side changes

#### In `SORTIE_MAIN` / `resol.F` (where GENANI is triggered)

Replace the blocking call to `SORTIE_MAIN` → `GENANI` with:

1. **Rank 0 only**: before entering `GENANI`, send the start signal to the I/O
   rank using `spmd_isend`.  The integer payload is the expected message count.
2. All compute ranks call a new `GENANI_ASYNC` which packs and sends all data,
   then returns immediately.
3. Compute ranks save the array of outstanding `request` handles in a
   module-level array.
4. At the next animation trigger (or at end-of-simulation), call
   `spmd_waitall` on those handles before sending the next frame.  This is the
   only synchronisation point on the compute side.

```fortran
! Pseudocode in resol.F / sortie_main.F
if (need_animation) then
  ! Wait for previous frame's sends to complete (handles from last time)
  if (num_anim_requests > 0) then
    call spmd_waitall(num_anim_requests, anim_requests)
    num_anim_requests = 0
  end if
  if (ISPMD == 0) then
    call spmd_isend(expected_msg_count, 1, IO_RANK, TAG_ANIM_START, req_ctrl)
    call spmd_wait(req_ctrl)
  end if
  call GENANI_ASYNC(..., anim_requests, num_anim_requests)
  ! return immediately — I/O rank now owns the data
end if
```

#### New `SPMD_GATHERF_ISEND` (replaces `SPMD_GATHERF` on compute ranks)

```fortran
! engine/source/mpi/anim/spmd_gatherf_isend.F90
subroutine SPMD_GATHERF_ISEND(V, WEIGHT, NODGLOB, requests, nreq)
  use spmd_mod, only: spmd_isend
  ! ... includes ...
  integer, intent(inout) :: requests(:)   ! grows per call
  integer, intent(inout) :: nreq

  ! Pack (same logic as current SPMD_GATHERF worker branch)
  SIZ = 0
  do I = 1, NUMNOD
    if (WEIGHT(I) == 1) then
      SIZ = SIZ + 1
      IBUF(SIZ) = NODGLOB(I)
      BUFSR(SIZ) = V(I)
    end if
  end do

  ! Non-blocking sends to I/O rank (IO_RANK) instead of rank 0
  nreq = nreq + 1
  call spmd_isend(IBUF,  SIZ, IO_RANK, TAG_ANIM_IDX,  requests(nreq))
  nreq = nreq + 1
  call spmd_isend(BUFSR, SIZ, IO_RANK, TAG_ANIM_REAL, requests(nreq))

  ! BUFSR and IBUF must remain valid until spmd_waitall is called.
  ! They are local allocations, so they must be promoted to a persistent
  ! module-level buffer (double-buffered if needed).
end subroutine
```

**Important**: `BUFSR` and `IBUF` are currently stack-allocated inside
`SPMD_GATHERF` and freed before the routine returns.  For non-blocking sends
the MPI standard requires the buffer to remain valid until `MPI_Wait`
completes.  They must therefore be promoted to **module-level persistent
allocations**, sized `NUMNODM`, and managed with a double-buffer (ping-pong)
so that frame N+1 packing does not overwrite frame N's in-flight buffers.

### I/O rank side: dedicated event loop

The I/O rank runs a simple event loop instead of the normal `resol` timestep.
It lives in a new subroutine, e.g. `ANIM_IO_RANK_LOOP`, called from
`radioss2.F` when `ISPMD == NSPMD`:

```fortran
! Pseudocode for the I/O rank loop
subroutine ANIM_IO_RANK_LOOP(NSPMD, NUMNODG, ...)
  use spmd_mod, only: spmd_irecv, spmd_waitany, spmd_recv, spmd_send
  integer :: ctrl_msg, completed, frame_count
  real,    allocatable :: XGLOB_R(:)
  integer, allocatable :: XGLOB_I(:)
  integer, allocatable :: recv_req(:,:)   ! (2, NSPMD) — idx + val per rank

  allocate(XGLOB_R(NUMNODG), XGLOB_I(NUMNODG))
  allocate(recv_req(2, NSPMD))

  do   ! event loop
    ! 1. Wait for frame-start signal from rank 0
    call spmd_recv(ctrl_msg, 1, rank0, TAG_ANIM_START)
    if (ctrl_msg == TAG_ANIM_STOP) exit

    ! expected_msgs = ctrl_msg (number of (idx,val) pairs to receive)
    ! 2. Open animation file, write header (rank 0 sent header metadata too)
    call OPEN_C(...)
    call WRITE_header(...)

    ! 3. For each field variable (the I/O rank knows the variable list
    !    because GENANI_ASYNC sends a small descriptor first):
    do var = 1, num_vars
      XGLOB_R = 1.0e6   ! sentinel

      ! Post irecv for all compute ranks simultaneously
      do I = 1, NSPMD
        call spmd_irecv(IBUF_R(I)%idx,  IBUF_R(I)%siz, IT_SPMD(I), TAG_ANIM_IDX,  recv_req(1,I))
        call spmd_irecv(IBUF_R(I)%vals, IBUF_R(I)%siz, IT_SPMD(I), TAG_ANIM_REAL, recv_req(2,I))
      end do

      ! Process completions as they arrive (spmd_waitany)
      do cnt = 1, NSPMD
        call spmd_waitany(NSPMD, recv_req(1,:), completed)
        I = completed
        call spmd_wait(recv_req(2,I))   ! matching value buffer
        ! Scatter into global array
        do K = 1, IBUF_R(I)%siz
          XGLOB_R(IBUF_R(I)%idx(K)) = IBUF_R(I)%vals(K)
        end do
      end do

      call WRITE_R_C(XGLOB_R, NUMNODG)
    end do

    call CLOSE_C(...)
  end do
end subroutine
```

Using `spmd_waitany` means the I/O rank can process and scatter data from each
compute rank as soon as it arrives, without waiting for the slowest sender.

### Buffer size concern (I/O rank)

The I/O rank must pre-allocate receive buffers of size `NUMNODM` per rank per
variable.  Since `NUMNODM` is the maximum local node count and is broadcast to
all ranks during initialisation (it is in `com04_c.inc` / `com01_c.inc`), the
I/O rank can query it with a small `spmd_recv` during initialisation.

Alternative: use `MPI_Probe` + `MPI_Get_count` as the current code does, which
avoids pre-allocation but breaks full non-blocking receive.  The recommended
approach is to pre-allocate.

### Tags for variable-typed data

Each call to `SPMD_GATHERF` currently uses the same tags (7014/7015).  With
async sends, in-flight messages from different variables must not interfere.
Options:

1. **Sequential discipline** (simplest): the I/O rank processes one variable
   at a time, posting all `NSPMD` receives for variable N before moving to N+1.
   Tags can remain 7014/7015 because the ordering is enforced by the I/O rank's
   loop structure.
2. **Variable-indexed tags**: encode the variable index into the tag, e.g.
   `TAG = 7000 + var_index * 2`. This allows the I/O rank to receive
   out-of-order but requires a larger tag space.

Option 1 is strongly recommended as the first implementation.

### Summary of file changes required

| File | Change |
|------|--------|
| `engine/source/mpi/anim/spmd_gatherf.F` | Add `SPMD_GATHERF_ISEND` variant; keep original for serial path |
| `engine/source/mpi/anim/spmd_gatheritab.F` | Same — add `SPMD_GATHERITAB_ISEND` variant |
| `engine/source/output/anim/generate/genani.F` | Add `GENANI_ASYNC` wrapper; replace `SPMD_GATHERF` calls with `SPMD_GATHERF_ISEND` inside it |
| `engine/source/output/sortie_main.F` | Manage `anim_requests` array; send `TAG_ANIM_START` to I/O rank |
| `engine/source/engine/resol.F` | Call `spmd_waitall` before next animation trigger; call `ANIM_IO_RANK_LOOP` for I/O rank |
| `engine/source/engine/radioss2.F` | Fork I/O rank into `ANIM_IO_RANK_LOOP` at startup |
| New: `engine/source/mpi/anim/anim_io_rank_mod.F90` | I/O rank event loop and persistent buffer management |

### Relationship to FVB (Flexible Volume Bag) routines

The FVB routines (`SPMD_FVB_AVEC`, `SPMD_FVB_ANOD`, …) use a delegation
pattern where a designated `PMAIN` process owns each bag and sends data to
rank 0.  For the async design, `PMAIN` would send to `IO_RANK` instead.
Since `PMAIN` may equal rank 0, the delegation logic is unchanged; only the
destination rank identifier changes from `IT_SPMD(1)` to `IT_SPMD(IO_RANK+1)`.

### Correctness requirements

1. **No compute-rank blocking**: `GENANI_ASYNC` must return before any
   `spmd_wait` on the compute side.  All waits happen at the start of the
   *next* animation cycle.
2. **I/O rank completeness**: the I/O rank must receive exactly the number of
   messages announced in `TAG_ANIM_START`.  A mismatch causes deadlock; add
   an assertion in debug mode.
3. **End-of-simulation flush**: at `TSTOP`, compute ranks call `spmd_waitall`
   on any outstanding animation sends, then rank 0 sends `TAG_ANIM_STOP` to
   the I/O rank before calling `MPI_Finalize`.
4. **NSPMD consistency**: within the domain-decomposition code, `NSPMD` still
   equals the number of compute ranks.  The I/O rank is invisible to all
   existing SPMD communication (force exchange, contact, etc.) because it
   does not participate in `SPMD_COMM_WORLD` for those operations — it only
   shares `SPMD_COMM_WORLD` for the animation messages.
