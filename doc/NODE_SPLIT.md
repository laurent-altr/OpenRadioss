# NODE_SPLIT – Non-Local Damage: Arrays and Implementation for Node Splitting

## Context

When a node is **split** (duplicated) to propagate a fracture, the entry point
is `detach_node` in `engine/source/engine/node_spliting/detach_node.F90`:

```fortran
call detach_node(nodes, node_id, elements, shell_list, list_size, &
                 npari, ninter, ipari, interf)
```

- `node_id` is the **local id** of the node being split.
- `shell_list(1:list_size)` are the shell **local ids** that will be detached
  from `node_id` and re-connected to the new node.
- On return, the new node has local id `nodes%numnod` (the last one; `numnod`
  was incremented inside the call).
- Elements **not** in `shell_list` keep `node_id`; elements in `shell_list`
  now reference the new node.

The actual call in `resol.F` is currently **commented out** (around line 5238).
The commented block also shows what the caller must do after the split:
re-initialise nodal state (`INIT_NODAL_STATE`), rebuild the general
element-to-node connectivity (`CHKINIT` for `ADDCNEL`/`CNEL`), and record the
new size of the mechanical PON (`LCNE0 = SIZE(ELEMENT%PON%PROCNE)`).

### What is already handled

`update_pon_shells` (called from `detach_node_from_shells`) already handles the
**mechanical PARITH/ON skyline** stored in `elements%pon` (type `element_pon_`
in `parith_on_mod.F90`):

| Updated field | Type |
|---|---|
| `elements%pon%adsky` | CSR row offsets for the mechanical force skyline |
| `elements%pon%iadc` | Per-shell FSKY row indices (mechanical) |
| `elements%pon%fsky` | Mechanical skyline force buffer |

### What is NOT yet handled

The **non-local damage structure `NLOCAL_STR_`** (`NLOC_DMG`) defined in
`common_source/modules/nlocal_reg_mod.F` has its own **independent** skyline
and DOF arrays.  These are completely separate allocatable arrays — not shared
with `elements%pon` — and are not touched anywhere in the existing node-
splitting code.  This document describes every field that must be patched and
gives the implementation steps.

---

## The `NLOCAL_STR_` type — field reference

Defined in `common_source/modules/nlocal_reg_mod.F`, instantiated as
`NLOC_DMG` throughout the engine.

### Key index relationships

```
non-local node rank N  (1 … NNOD)
  ↕  INDX(N)  →  local node id
  ↕  IDXI(local_id)  →  N   (inverse, size NUMNOD)

non-local DOF range for node N:
  first DOF = POSI(N),  last DOF = POSI(N+1)-1
  NDDL(N)   = POSI(N+1) - POSI(N)
  L_NLOC    = POSI(NNOD+1) - 1   (total DOF count)

element-to-node skyline (PARITH/ON):
  ADDCNE(N) … ADDCNE(N+1)-1  →  row range in FSKY for contributions to node N
  PROCNE(k) = MPI rank of the contributing element
  FSKY (ADDCNE(NNOD+1), NDDMAX)  →  non-local force accumulator for assembly
  STSKY(ADDCNE(NNOD+1), NDDMAX)  →  non-local stiffness accumulator
  CNE  — always allocated at size 0; unused at runtime

per-element back-pointers (PARITH/ON):
  IADS(1:8, e)  →  8 FSKY row indices for solid element e
  IADC(1:4, e)  →  4 FSKY row indices for shell element e
  IADTG(1:3,e)  →  3 FSKY row indices for 3-node shell element e
```

### Scalar counters

| Field | Meaning |
|-------|---------|
| `IMOD` | Non-local regularisation flag (>0 = active) |
| `NNOD` | Number of non-local nodes |
| `L_NLOC` | Total length of DOF vectors |
| `LCNE_NL` | Length of `PROCNE`/`FSKY` first dimension (same as `ADDCNE(NNOD+1)-1`) |
| `NUMELS_NL` | Number of non-local solid elements |
| `NUMELC_NL` | Number of non-local shell elements |
| `NUMELTG_NL` | Number of non-local 3-node shell elements |
| `NDDMAX` | Max number of DOFs per node (second dim of `FSKY`/`STSKY`) |

### Node-index tables

| Array | Dim | Meaning | Change on split |
|-------|-----|---------|----------------|
| `INDX` | `(NNOD)` | Non-local rank → local node id | Append new local id |
| `POSI` | `(NNOD+1)` | CSR DOF offsets | Append new entry (`L_NLOC + 1`); last entry = new `L_NLOC` |
| `IDXI` | `(NUMNOD)` | Local node id → non-local rank | Extend by 1; set `IDXI(new_id) = new_NNOD` |

> `NUMNOD` grows by 1 after the split; `IDXI` must be reallocated.
> The parent node entry in `IDXI` is unchanged.

### DOF-space state vectors

All sized `(L_NLOC)` or `(L_NLOC, NTHREAD)`.  The new node inherits the
parent's DOF values for continuous state fields; force/stiffness accumulators
are reset to zero.

| Array | Dim | Meaning | Initialise new DOFs |
|-------|-----|---------|---------------------|
| `MASS` | `(L_NLOC)` | Non-local mass | Copy from parent, then split both by ½ (same convention as `nodes%MS`) |
| `MASS0` | `(L_NLOC)` | Reference non-local mass | Copy from parent, split by ½ |
| `VNL` | `(L_NLOC)` | Non-local variable velocity | Copy from parent |
| `VNL_OLD` | `(L_NLOC)` | Previous-step velocity | Copy from parent |
| `DNL` | `(L_NLOC)` | Increment | Copy from parent |
| `UNL` | `(L_NLOC)` | Cumulated variable | Copy from parent |
| `FNL` | `(L_NLOC, NTHREAD)` | Force / acceleration buffer | Zero |
| `STIFNL` | `(L_NLOC, NTHREAD)` | Nodal stiffness buffer | Zero |

### Skyline connectivity (PARITH/ON only, `IPARI0 == 1`)

These are analogous to `elements%pon%adsky / iadc / fsky` but are **separate,
independent allocatable arrays** inside `NLOC_DMG`.  They are filled by the
element force routines (e.g. `cfint_reg`): each shell writes its non-local
force contribution into `NLOC_DMG%FSKY(NLOC_DMG%IADC(j,shell_id), dof)`, and
`ASSPAR_SUB` then gathers from `NLOC_DMG%FSKY` into `NLOC_DMG%FNL` using
`NLOC_DMG%ADDCNE`.  `SPMD_EXCH_SUB_PON` uses `ADDCNE` and `PROCNE` to build
the MPI send/receive indices (see §MPI arrays).

> **`CNE`** — always allocated with size 0 everywhere in the engine and never
> read during the time loop.  It does **not** need to be updated on a split.

| Array | Dim | Meaning | Change on split |
|-------|-----|---------|----------------|
| `ADDCNE` | `(NNOD+1)` | CSR row offsets into the FSKY accumulator for each non-local node | Extend by 1 for new node; set new entry from contribution count in `shell_list` |
| `PROCNE` | `(LCNE_NL)` | MPI rank of each element contribution (parallel to FSKY rows) | Append alongside new FSKY rows |
| `FSKY` | `(ADDCNE(NNOD+1), NDDMAX)` | Non-local skyline force accumulator | Extend; new entries initialised to zero |
| `STSKY` | `(ADDCNE(NNOD+1), NDDMAX)` | Non-local skyline stiffness accumulator | Extend; new entries initialised to zero |
| `IADC` | `(4, NUMELC_NL)` | Per-shell FSKY row indices for non-local assembly | Update for shells in `shell_list` where the corner node was `node_id` |
| `IADS` | `(8, NUMELS_NL)` | Per-solid FSKY row indices | Update for any solid in `shell_list` |
| `IADTG` | `(3, NUMELTG_NL)` | Per-3n-shell FSKY row indices | Update for any 3-node shell in `shell_list` |

`LCNE_NL` must be updated to `ADDCNE(new_NNOD+1) - 1` after the extension.

### MPI / SPMD boundary arrays

These arrays are computed by `SPMD_EXCH_SUB_PON` (`engine/source/mpi/spmd_exch_sub.F`)
from `ADDCNE`, `PROCNE`, `IAD_ELEM`, `FR_ELEM`, and `POSI`.  They must be
**fully rebuilt** after a split by calling `SPMD_EXCH_SUB_PON(NLOC_DMG)` again —
localised patching is not feasible because the send/receive patterns depend
on the global topology.  The call is guarded by `IPARIT == 1`, so it only runs
in the PARITH/ON branch (same as the rest of the skyline logic).

| Array | Dim | Meaning |
|-------|-----|---------|
| `IAD_ELEM` | `(NSPMD+1)` | Offset into `FR_ELEM` per domain |
| `IAD_SIZE` | `(NSPMD+1)` | Buffer size at each domain boundary |
| `FR_ELEM` | `(TOTAL_NODES)` | Non-local node list at SPMD boundaries |
| `ISENDSP` | variable | Skyline PON send index (built by `SPMD_EXCH_SUB_PON`) |
| `IRECSP` | variable | Skyline PON receive index (built by `SPMD_EXCH_SUB_PON`) |
| `IADSDP` | `(NSPMD+1)` | PARITH/ON send displacement (built by `SPMD_EXCH_SUB_PON`) |
| `IADRCP` | `(NSPMD+1)` | PARITH/ON receive displacement (built by `SPMD_EXCH_SUB_PON`) |
| `FR_NBCC` | `(2, NSPMD)` | Node count exchanged per domain pair (built by `SPMD_EXCH_SUB_PON`) |
| `FR_ELEM_S` | variable | Elements to send at boundaries (built by `SPMD_EXCH_SUB_PON`) |
| `FR_ELEM_R` | variable | Elements to receive at boundaries (built by `SPMD_EXCH_SUB_PON`) |

### Material-property arrays — no change needed

Sized `(NUMMAT)`, independent of node count.

| Array | Meaning |
|-------|---------|
| `DENS` | Non-local pseudo-density per material |
| `DAMP` | Non-local damping coefficient per material |
| `LEN` | Non-local internal length per material |
| `LE_MAX` | Max element length target per material |
| `SSPNL` | Non-local sound speed per material |

---

## Implementation guide

### 1 — Where to hook in

Add a call to the new subroutine `detach_node_nloc` (see §2) **inside
`detach_node`** in `detach_node.F90`, after `detach_node_from_shells` has
updated the element connectivity and before `nodes%numnod` is incremented:

```fortran
subroutine detach_node(nodes, node_id, elements, shell_list, list_size, &
                       npari, ninter, ipari, interf)
  ...
  call detach_node_from_interfaces(...)
  call extend_nodal_arrays(nodes, numnod+1)
  call set_new_node_values(nodes, i)
  call detach_node_from_shells(nodes, node_id, elements, shell_list, list_size)

  ! ---- NEW: update non-local damage structure ----
  if (nloc_dmg%imod > 0) then
    call detach_node_nloc(nloc_dmg, node_id, nodes%numnod+1, &
                          shell_list, list_size)
  end if
  ! ------------------------------------------------

  nodes%numnod = nodes%numnod + 1
end subroutine detach_node
```

`NLOC_DMG` must be threaded into `detach_node` as an additional
`type(NLOCAL_STR_), intent(inout)` argument, and passed through the call chain
from `test_jc_shell_detach` (and any future caller) in `resol.F`.

### 2 — New subroutine `detach_node_nloc`

Create `engine/source/engine/node_spliting/detach_node_nloc.F90`.

#### Signature

```fortran
subroutine detach_node_nloc(nloc_dmg, old_local_id, new_local_id, &
                             shell_list, list_size)
  use nlocal_reg_mod
  use my_alloc_mod
  use extend_array_mod
  implicit none
  type(nlocal_str_), intent(inout) :: nloc_dmg
  integer,           intent(in)    :: old_local_id   !< local id of the split node
  integer,           intent(in)    :: new_local_id   !< local id of the new node (= old numnod + 1)
  integer,           intent(in)    :: list_size
  integer,           intent(in)    :: shell_list(list_size)
```

#### Step-by-step logic

```
Step 1 — Early exit
  nl_idx = nloc_dmg%idxi(old_local_id)
  if (nl_idx == 0) return   ! parent is not a non-local node

Step 2 — DOF layout of the parent
  old_pos = nloc_dmg%posi(nl_idx)
  nddl    = nloc_dmg%posi(nl_idx+1) - old_pos   ! DOF count

Step 3 — Extend IDXI (size NUMNOD → NUMNOD+1)
  call extend_array(nloc_dmg%idxi, old_numnod, new_local_id)
  nloc_dmg%idxi(new_local_id) = nloc_dmg%nnod + 1

Step 4 — Extend INDX (size NNOD → NNOD+1)
  call extend_array(nloc_dmg%indx, nloc_dmg%nnod, nloc_dmg%nnod+1)
  nloc_dmg%indx(nloc_dmg%nnod+1) = new_local_id

Step 5 — Extend POSI (size NNOD+1 → NNOD+2)
  call extend_array(nloc_dmg%posi, nloc_dmg%nnod+1, nloc_dmg%nnod+2)
  nloc_dmg%posi(nloc_dmg%nnod+2) = nloc_dmg%l_nloc + nddl + 1
    ! posi is 1-based: new node's DOFs start at l_nloc+1

Step 6 — Extend DOF-space vectors by nddl
  new_pos = nloc_dmg%l_nloc + 1
  For each of MASS, MASS0, VNL, VNL_OLD, DNL, UNL:
    call extend_array(array, nloc_dmg%l_nloc, nloc_dmg%l_nloc + nddl)
    array(new_pos : new_pos+nddl-1) = array(old_pos : old_pos+nddl-1)
  ! Split mass equally (same as nodes%MS split in set_new_node_values)
  nloc_dmg%mass (new_pos:new_pos+nddl-1) = nloc_dmg%mass (old_pos:old_pos+nddl-1) * HALF
  nloc_dmg%mass0(new_pos:new_pos+nddl-1) = nloc_dmg%mass0(old_pos:old_pos+nddl-1) * HALF
  nloc_dmg%mass (old_pos:old_pos+nddl-1) = nloc_dmg%mass (old_pos:old_pos+nddl-1) * HALF
  nloc_dmg%mass0(old_pos:old_pos+nddl-1) = nloc_dmg%mass0(old_pos:old_pos+nddl-1) * HALF
  For FNL (l_nloc, nthread) and STIFNL (l_nloc, nthread):
    call extend_array_2d(array, nloc_dmg%l_nloc, nthread, nloc_dmg%l_nloc+nddl, nthread)
    array(new_pos:new_pos+nddl-1, 1:nthread) = ZERO

Step 7 — Commit scalar counters
  nloc_dmg%nnod   = nloc_dmg%nnod + 1
  nloc_dmg%l_nloc = nloc_dmg%l_nloc + nddl

Step 8 — Skyline update (PARITH/ON only)
  new_nnod = nloc_dmg%nnod   ! already incremented
  Count contributions from shell_list to the new node (= number of
  (shell, corner) pairs in shell_list where corner was old_local_id,
  now replaced by new_local_id in elements%shell%nodes).
  Call this count n_contrib.

  8a — Extend ADDCNE by 1:
    call extend_array(nloc_dmg%addcne, new_nnod, new_nnod+1)
    nloc_dmg%addcne(new_nnod+1) = nloc_dmg%addcne(new_nnod) + n_contrib

  8b — Extend PROCNE by n_contrib and append entries:
    old_lcne = nloc_dmg%lcne_nl
    call extend_array(nloc_dmg%procne, old_lcne, old_lcne + n_contrib)
    k = old_lcne + 1
    do i = 1, list_size
      shell_id = shell_list(i)
      do j = 1, 4
        if elements%shell%nodes(j, shell_id) == new_local_id:
          nloc_dmg%procne(k) = ISPMD   ! local domain rank
          k = k + 1
      end do
    end do
    nloc_dmg%lcne_nl = old_lcne + n_contrib
    ! NOTE: CNE is always size 0 and unused; do NOT extend it.

  8c — Extend FSKY and STSKY rows by n_contrib, zeroing new entries:
    old_fsky_rows = nloc_dmg%addcne(new_nnod)   ! rows before new node's block
    new_fsky_rows = nloc_dmg%addcne(new_nnod+1)
    call extend_array_2d(nloc_dmg%fsky,  old_fsky_rows, nddmax, new_fsky_rows, nddmax)
    call extend_array_2d(nloc_dmg%stsky, old_fsky_rows, nddmax, new_fsky_rows, nddmax)
    nloc_dmg%fsky (old_fsky_rows+1:new_fsky_rows, 1:nddmax) = ZERO
    nloc_dmg%stsky(old_fsky_rows+1:new_fsky_rows, 1:nddmax) = ZERO

  8d — Update IADC back-pointers for shells in shell_list:
    For each shell_id in shell_list, for each corner j = 1..4:
      if elements%shell%nodes(j, shell_id) == new_local_id:
        nloc_dmg%iadc(j, shell_id) = <new FSKY row assigned in 8c>
    The assignment must use the same ordering as used when filling PROCNE in 8b.
```

### 3 — SPMD boundary arrays

The arrays `ISENDSP`, `IRECSP`, `IADSDP`, `IADRCP`, `FR_NBCC`, `FR_ELEM_S`,
`FR_ELEM_R` are built by `SPMD_EXCH_SUB_PON` (`engine/source/mpi/spmd_exch_sub.F`)
from `ADDCNE`, `PROCNE`, `IAD_ELEM`, `FR_ELEM`, and `POSI`.  They must be
**fully rebuilt** after the split by calling:

```fortran
if (nloc_dmg%imod > 0 .and. iparit == 1 .and. nspmd > 1) then
  call spmd_exch_sub_pon(nloc_dmg)
end if
```

This call should be placed in `resol.F` alongside the mechanical PON rebuild
that happens after all node splits in a cycle (see commented block near line
5238).  A localised patch is not feasible because the patterns depend on the
global non-local node layout across all MPI domains.

### 4 — Restart compatibility

`write_nloc_struct.F` and `read_nloc_struct.F` write/read `NNOD`, `L_NLOC`,
`LCNE_NL`, `NUMELS_NL`, `NUMELC_NL`, `NUMELTG_NL`, `NDDMAX` as a header, then
all arrays sequentially.  After a node split the sizes change; no extra work is
needed beyond ensuring the write happens after the split update so that the
checkpoint captures the new state.
