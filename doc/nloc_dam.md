# Non-Local Damage – Data Structure and Algorithms

This document describes the non-local damage regularisation subsystem in OpenRadioss
(engine + starter), with a focus on the data types, the key arrays, the main
processing routines, and guidance on how to derive a **crack node** and **crack
direction** from the non-local state for the node-splitting erosion strategy.

---

## 1. Purpose of the Non-Local Damage Model

The non-local damage approach avoids mesh-dependence of softening laws by
replacing the local state variable (typically cumulated plastic strain) driving
failure with a regularised, spatially smoothed value `ε̃`.  The smoothing is
implemented as an implicit gradient model: an auxiliary field `ε̃` is added on
each structural node as one or more extra degrees of freedom (one per
through-thickness integration point for shells, one for solids).  `ε̃` is
governed by a second-order PDE in space (the Helmholtz equation with internal
length `L`), solved explicitly in pseudo-time with an inertia term (density
`ρ_NL`), a damping term (`ξ`), and driven by the local variable `ε_loc` as a
source.

---

## 2. Main Data Type: `NLOCAL_STR_`

Defined in `common_source/modules/nlocal_reg_mod.F`, module `NLOCAL_REG_MOD`.

### 2.1 Scalar flags and counts

| Field | Fortran type | Meaning |
|---|---|---|
| `IMOD` | `integer` | Master switch: `0` = non-local inactive, `1` = active |
| `NNOD` | `integer` | Number of structural nodes that carry non-local DOFs |
| `L_NLOC` | `integer` | Total length of the 1-D non-local DOF vectors (= `SUM(NDDL)` over all non-local nodes) |
| `NUMELS_NL` | `integer` | Number of non-local *solid* elements |
| `NUMELC_NL` | `integer` | Number of non-local *quad-shell* elements |
| `NUMELTG_NL` | `integer` | Number of non-local *triangle-shell* elements |
| `NDDMAX` | `integer` | Maximum DOF count per node (= max number of through-thickness integration points) |
| `LCNE_NL` | `integer` | Current size of the `PROCNE` array (total element-corner contributions in skyline) |

### 2.2 Material parameters (one entry per material, size `NUMMAT`)

| Field | Meaning |
|---|---|
| `DENS(NUMMAT)` | Non-local pseudo-density ρ_NL (auto-computed, ensures stability w.r.t. time step) |
| `DAMP(NUMMAT)` | Non-local damping coefficient ξ |
| `LEN(NUMMAT)` | Non-local internal length L (user-defined) |
| `LE_MAX(NUMMAT)` | Characteristic element size L_e used to auto-set ρ_NL |
| `SSPNL(NUMMAT)` | Non-local "sound speed" (governs how fast regularisation propagates) |

### 2.3 Node index tables

These tables relate global node local IDs to positions inside the 1-D
non-local DOF vectors.

| Field | Size | Meaning |
|---|---|---|
| `INDX(NNOD)` | `NNOD` | `INDX(n)` = local node ID of the *n*-th non-local node.  Maps non-local rank → node |
| `IDXI(NUMNOD)` | `NUMNOD` | Inverse: `IDXI(i)` = non-local rank of node `i`; `0` if node has no non-local DOFs |
| `POSI(NNOD+1)` | `NNOD+1` | CSR-style offsets: DOFs of non-local node `n` occupy `[POSI(n) … POSI(n+1)-1]` in all L_NLOC-sized vectors.  `NDDL(n) = POSI(n+1) - POSI(n)` |

**Usage pattern** (very frequent throughout the engine):
```fortran
nl_idx = nloc_dmg%idxi(node_local_id)  ! 0 if node is not non-local
pos    = nloc_dmg%posi(nl_idx)
nddl   = nloc_dmg%posi(nl_idx + 1) - pos
! DOFs are nloc_dmg%unl(pos : pos+nddl-1)
```

### 2.4 State vectors (size `L_NLOC`)

All state vectors are indexed in the same DOF space defined by `POSI`.

| Field | Meaning |
|---|---|
| `MASS(L_NLOC)` | Non-local mass (current, may be reduced when elements break) |
| `MASS0(L_NLOC)` | Non-local mass at t = 0 (reference, never modified after init) |
| `UNL(L_NLOC)` | **Regularised (non-local) cumulated variable** ε̃ — the output field |
| `DNL(L_NLOC)` | Increment of ε̃ at current time step |
| `VNL(L_NLOC)` | "Velocity" of ε̃ (dε̃/dt in the explicit pseudo-time integration) |
| `VNL_OLD(L_NLOC)` | Value of `VNL` at the beginning of the step |
| `FNL(L_NLOC, NTHREAD)` | Non-local "force" accumulated by element contributions; thread-safe (one column per thread in PARITH/OFF) |
| `STIFNL(L_NLOC, NTHREAD)` | Non-local equivalent nodal stiffness (used for time-step control) |

### 2.5 Skyline connectivity (PARITH/ON path)

Used when parallel assembly is enabled (`IPARIT /= 0`).  The skyline stores
element contributions at addresses indexed by element corner.

| Field | Size | Meaning |
|---|---|---|
| `ADDCNE(0:NNOD+1)` | `NNOD+2` | CSR row offsets for the skyline: contributions of non-local node `n` are at rows `ADDCNE(n) … ADDCNE(n+1)-1` in `FSKY`/`STSKY` |
| `CNE(ADDCNE(NNOD+1)-1)` | variable | Reserved; always allocated at size 0 (legacy, not used in current code) |
| `PROCNE(LCNE_NL)` | `LCNE_NL` | MPI rank that owns each element-corner contribution |
| `IADS(4, NUMELS_NL)` | 2-D | Back-pointer: `IADS(corner, elem)` = row in `FSKY`/`STSKY` for that solid element corner |
| `IADC(4, NUMELC_NL)` | 2-D | Same for quad-shell elements (4 corners) |
| `IADTG(3, NUMELTG_NL)` | 2-D | Same for triangle-shell elements (3 corners) |
| `FSKY(rows, NDDMAX)` | 2-D | Skyline accumulator for non-local forces |
| `STSKY(rows, NDDMAX)` | 2-D | Skyline accumulator for non-local stiffness |

### 2.6 MPI boundary and communication arrays

These fields are stored inside `NLOCAL_STR_`, but they are **derived** from the
structural boundary lists in `NODES%BOUNDARY` / `NODES%BOUNDARY_ADD`:

| Field | Meaning |
|---|---|
| `NODES%BOUNDARY` | Flat list of structural boundary node IDs |
| `NODES%BOUNDARY_ADD` | Offsets into `NODES%BOUNDARY` per MPI domain |

`SPMD_SUB_BOUNDARIES` rebuilds the `NLOCAL_STR_` tables below from those nodal
boundary lists.

| Field | Meaning |
|---|---|
| `IAD_ELEM(NSPMD+1)`, `IAD_SIZE(NSPMD+1)` | SPMD boundary element list offsets/sizes |
| `FR_ELEM(TOTAL_NODES)` | MPI frontier element list |
| `ISENDSP`, `IRECSP` | Skyline PON send/receive index tables |
| `IADSDP(NSPMD+1)`, `IADRCP(NSPMD+1)` | Communication buffer offsets for PARITH/ON |
| `FR_NBCC(2, NSPMD)`, `FR_ELEM_S`, `FR_ELEM_R` | PARITH/ON frontier communication tables |

---

## 3. Key Routines

### 3.1 Starter – Initialisation

#### `NLOC_DMG_INIT` (`starter/source/materials/fail/nloc_dmg_init.F`)

Called from `INITIA`. Builds all of the above arrays from scratch:

1. Scans every element group flagged `IPARG(78,NG) > 0` (non-local flag).
2. Computes element volumes, tags nodes, counts the DOF per node (`NDDL`).
3. Fills `INDX`, `IDXI`, `POSI`.
4. Auto-computes `DENS`, `DAMP`, `SSPNL` per material using element sizes
   and the stability criterion `DENS ≥ CSTA * ((L/L_e)² + 1/12) * Δt_min²`.
5. Allocates and fills `MASS`, `MASS0` (weighted by element volume and
   through-thickness Gauss weights).
6. Allocates zero-filled `UNL`, `VNL`, `VNL_OLD`, `DNL`, `FNL`, `STIFNL`.

#### `NLOCAL_INIT_STA` (`starter/source/materials/fail/nlocal_init_sta.F`)

Optional static pre-equilibration of the non-local field: runs explicit
pseudo-time steps to convergence before the first time step, so that ε̃ starts
consistent with the initial local damage state.

### 3.2 Engine – Time integration

The non-local field is integrated with a central-difference explicit scheme,
mirroring the mechanical time integration.  Each engine time step calls (in
`resol.F`):

| Subroutine | File | Action |
|---|---|---|
| `NLOCAL_VEL` | `engine/source/assembly/nlocal_vel.F` | Advance `VNL` by half step: `VNL += DT12 * FNL/MASS`; save `VNL_OLD`; reset `FNL = 0` |
| `NLOCAL_INCR` | `engine/source/assembly/nlocal_incr.F` | Compute increment: `DNL = DT2 * VNL`; update: `UNL += DNL` |
| `NLOCAL_ACC` | `engine/source/assembly/nlocal_acc.F` | Convert force to acceleration: `FNL /= MASS` |

Between `NLOCAL_VEL` and `NLOCAL_ACC`, element loops call `cfint_reg`
(and its variants for other element types) to accumulate `FNL` from the
element contributions.

### 3.3 Engine – Element contribution to `FNL`

#### `CFINT_REG` (`engine/source/elements/shell/coque/cfint_reg.F`)

The prototypical element force routine for quad shells.  For each element and
each through-thickness DOF `k`:

```
F_i(k) = WF(k) * [ L² ∇ᵀB_i ∇ε̃  +  (1/NTN) * V * (ε̃_avg - ε_loc) ]
```

Where:
- `L²` = `LEN²`  
- `BTB11/12/22` = element stiffness geometry terms (`B^T B` matrix)
- `UNL(POS_i+k-1)` = ε̃ at node i, DOF k (the non-local regularised variable)
- `VAR_REG(i,k)` = local source variable (typically cumulated plastic strain)
- If an element is **broken** (`OFF ≤ 0`), absorbing forces are applied instead
  (wave-absorbing boundary condition using `SSPNL * ZETA * VNL`)

The result is scattered into `FNL` (PARITH/OFF) or `FSKY` (PARITH/ON) using
the `POSI`/`IADC` pointers.

Analogous routines exist for other element types:
- `C3FINT_REG` / `CDKFINT_REG` — triangle shells  
- `CBAFINT_REG` — Batoz-Dhatt shells  
- `SFINT_REG` / `S4FINT_REG` / `S8ZFINT_REG` — 3-D solid elements  

### 3.4 Engine – Activating node splitting (`resol.F` ~line 5236)

The node-splitting block in `resol.F` is active and calls, in order:

1. `test_jc_shell_detach` — detect and perform splits (calls `detach_node` → `detach_node_nloc`)
2. `INIT_NODAL_STATE` — re-initialise nodal boundary/state arrays for the new node
3. `CHKINIT` (gated on ALE/deletion flags) — rebuild element-to-node connectivity skyline
4. `ALLOCATE_OUTPUT_DATA` — extend output arrays for the new node
5. `SPMD_SUB_BOUNDARIES(NLOC_DMG, NODES%BOUNDARY_ADD, NODES%BOUNDARY)` (gated on
   `NSPMD > 1`) — rebuild the non-local MPI boundary tables after the split

This is the important fix for the multi-domain case: after node splitting the
topology changes, so the boundary tables must be rebuilt rather than just
exchanged.  The current code now does that directly in `resol.F`.

### 3.5 Engine – Node splitting support

#### `detach_node_nloc` (`engine/source/engine/node_spliting/detach_node_nloc.F90`)

Called from `detach_node` immediately after `detach_node_from_shells`, before
`nodes%numnod` is incremented.  It extends all non-local arrays to accommodate
the new (duplicated) node:

1. Early exit if the parent node has no non-local DOFs (`IDXI == 0`).
2. Extends `IDXI`, `INDX`, `POSI` by one entry.
3. Extends all DOF-space vectors (`MASS`, `MASS0`, `VNL`, `VNL_OLD`, `DNL`,
   `UNL`, `FNL`, `STIFNL`) by `NDDL` entries, copying the parent's state.
4. **Splits mass equally** between parent and child (conserves total non-local
   mass, which is proportional to the volume carried by each node).
5. If the PARITH/ON skyline is allocated (`ADDCNE` is allocated):
   - Counts the element-corner contributions now assigned to the new node.
   - Extends `ADDCNE`, `PROCNE`, `FSKY`, `STSKY`.
   - Updates `IADC` back-pointers for the detached shells.

After a node split the post-processing block in `resol.F` (~line 5285) now contains:

```fortran
! Rebuild non-local SPMD boundary arrays (ISENDSP, IRECSP, etc.)
! after the skyline topology changed due to node split.
IF (NSPMD > 1 .AND. NLOC_DMG%IMOD > 0) THEN
  CALL SPMD_SUB_BOUNDARIES(NLOC_DMG, NODES%BOUNDARY_ADD, NODES%BOUNDARY)
ENDIF
```

This rebuilds `NLOC_DMG%IAD_ELEM`, `NLOC_DMG%IAD_SIZE`, and `NLOC_DMG%FR_ELEM`
from the updated nodal boundary tables, so the non-local MPI state stays valid
after the split.

---

## 4. Array Size Summary

```
NNOD        : number of non-local nodes  (subset of all nodes)
L_NLOC      : sum of NDDL over all non-local nodes
NDDL(n)     : through-thickness DOF count of node n (= NPTT for shells)
NDDMAX      : max(NDDL)

POSI        : size NNOD+1   (CSR offsets into L_NLOC space)
INDX        : size NNOD     (non-local rank → node local id)
IDXI        : size NUMNOD   (node local id → non-local rank, 0 if not NL)

All L_NLOC vectors (MASS, UNL, VNL, DNL, ...): size L_NLOC
FNL, STIFNL : size L_NLOC × NTHREAD

ADDCNE      : size NNOD+2   (skyline CSR offsets, PARITH/ON only)
FSKY, STSKY : size ADDCNE(NNOD+1) × NDDMAX
IADC        : size 4 × NUMELC_NL
IADS        : size 4 × NUMELS_NL
IADTG       : size 3 × NUMELTG_NL
```

---

## 5. How to Derive a Crack Node and Crack Direction for Node Splitting

The non-local field `UNL` is exactly the information needed to drive
node-splitting: the **highest-damage node** and **the orientation of the
damage gradient** (perpendicular to the crack plane) are both directly
accessible from `NLOC_DMG`.

### 5.1 Identifying the crack node

For each structural node `i` belonging to non-local elements:

```fortran
nl_idx = nloc_dmg%idxi(i)
if (nl_idx == 0) cycle          ! node not non-local
pos  = nloc_dmg%posi(nl_idx)
nddl = nloc_dmg%posi(nl_idx+1) - pos
unl_node = maxval(nloc_dmg%unl(pos : pos+nddl-1))
```

`unl_node` is the peak regularised strain at node `i` across the thickness.
The node to split is the one maximising `unl_node` among all candidate nodes
(e.g. nodes shared by at least two heavily loaded elements), subject to the
condition that the value exceeds the failure threshold of the material.

For shells with `NDDL > 1`, the through-thickness DOF index `k` that achieves
the maximum indicates **which layer** is most damaged (useful for progressive
delamination detection).

### 5.2 Deriving the crack direction

The implicit-gradient regularisation means that `UNL` is smooth.  The **crack
plane normal** is parallel to `∇UNL` evaluated at the crack node: the crack
grows perpendicular to the direction of maximum gradient.

**Practical computation** using the nodal values already stored in `UNL`:

For a quad shell, the gradient within element `e` at its centroid is:

```
∇UNL_e ≈ (1/A_e) * SUM_{corner i} [UNL_i * outward_normal_of_opposite_edge * edge_length_i]
```

or equivalently using the `BTB` geometry matrices already computed inside
`cfint_reg`:

```
grad_x_e(k) = (1/V_e) * (BTB11 * (UNL_1 - UNL_3) + BTB12 * (UNL_2 - UNL_4))
grad_y_e(k) = (1/V_e) * (BTB12 * (UNL_1 - UNL_3) + BTB22 * (UNL_2 - UNL_4))
```

The **crack normal** at the target node is the weighted average of `∇UNL_e`
over the elements sharing that node:

```
n_crack = NORMALIZE( SUM_e [ vol_e * ∇UNL_e ] )
```

The **crack plane** passes through the node and is perpendicular to `n_crack`.
The **edge to split** between two adjacent nodes `A` and `B` is the one most
parallel to `n_crack` (i.e. maximising `|dot(AB, n_crack)|`).

### 5.3 Suggested implementation sketch

```fortran
! 1. Find the highest-damage non-local node
unl_max = 0.0_wp
split_node = 0
do i = 1, numnod
  nl_idx = nloc_dmg%idxi(i)
  if (nl_idx == 0) cycle
  pos  = nloc_dmg%posi(nl_idx)
  nddl = nloc_dmg%posi(nl_idx+1) - pos
  val  = maxval(nloc_dmg%unl(pos:pos+nddl-1))
  if (val > unl_threshold .and. val > unl_max) then
    unl_max    = val
    split_node = i
  end if
end do

! 2. Compute ∇UNL at split_node from attached element DOFs
crack_normal = 0.0_wp
total_weight = 0.0_wp
do ie = 1, n_elems_attached_to_split_node
  e = attached_elem(ie)
  ! retrieve UNL at each corner of element e
  do j = 1, 4
    inod  = ixc(j+1, e)
    nl_j  = nloc_dmg%idxi(inod)
    pos_j = nloc_dmg%posi(nl_j)
    unl_corner(j) = nloc_dmg%unl(pos_j)   ! DOF k=1 for simplicity
  end do
  ! approximate in-plane gradient (using element metric)
  grad(1) = (unl_corner(1) - unl_corner(3)) / dx_e
  grad(2) = (unl_corner(2) - unl_corner(4)) / dy_e
  weight   = element_area(e)
  crack_normal = crack_normal + weight * grad
  total_weight = total_weight + weight
end do
crack_normal = crack_normal / total_weight
crack_normal = crack_normal / norm2(crack_normal)   ! unit normal

! 3. The crack plane bisects the node; pick the element edge most perpendicular
!    to crack_normal (i.e. the edge most aligned with the crack plane) to guide
!    which elements go to the new node
```

### 5.4 Relationship with the existing `test_jc_shell_detach` implementation

The current implementation in `nloc_shell_detach` (`nloc_shell_detach.F90`)
already uses the **non-local field** `UNL` directly to build the shell damage
indicator, instead of the older `dammx` / `OFF` path:

| Criterion | Source | When available |
|---|---|---|
| `dammx` / `OFF` | Local failure flag in element buffer | After failure criterion is met (post-peak) |
| `UNL` | Non-local regularised variable | From the onset of inelastic strains |

This means the split trigger is now aligned with the regularised damage field,
so node splitting can happen **before** the element actually fails. That better
captures the damage-band geometry and avoids relying only on element deletion.

What is still not derived from `UNL` is the **crack direction / split plane
normal**: the current implementation still uses the shell-geometry eccentricity
heuristic to choose which corner to detach.

---

## 6. File Index

| File | Role |
|---|---|
| `common_source/modules/nlocal_reg_mod.F` | Definition of `NLOCAL_STR_` and `NLOCAL_REG_MOD`; utility routines `GET_LEMAX`, `GET_LENGTH` |
| `starter/source/materials/fail/nloc_dmg_init.F` | Allocate and fill all non-local arrays at startup |
| `starter/source/materials/fail/nlocal_init_sta.F` | Optional static pre-equilibration |
| `starter/source/materials/nonlocal/hm_read_nonlocal.F` | Read non-local parameters from input deck; sets `IMOD = 1` |
| `engine/source/assembly/nlocal_vel.F` | Half-step velocity update |
| `engine/source/assembly/nlocal_incr.F` | Increment and displacement update |
| `engine/source/assembly/nlocal_acc.F` | Force-to-acceleration conversion |
| `engine/source/elements/shell/coque/cfint_reg.F` | Quad-shell contribution to `FNL` |
| `engine/source/elements/sh3n/coque3n/c3fint_reg.F` | Triangle-shell contribution |
| `engine/source/elements/shell/coqueba/cbafint_reg.F` | Batoz-Dhatt shell contribution |
| `engine/source/elements/solid/solide/sfint_reg.F` | Solid element contribution |
| `engine/source/time_step/nlocal_dtnoda.F` | Non-local nodal time-step control |
| `engine/source/engine/node_spliting/detach_node_nloc.F90` | Extend `NLOC_STR_` when a node is split |
| `engine/source/engine/node_spliting/detach_node.F90` | Top-level node-splitting driver; calls `detach_node_nloc` |
| `engine/source/output/restart/write_nloc_struct.F` | Write non-local arrays to restart file |
| `engine/source/output/restart/read_nloc_struct.F` | Read non-local arrays from restart file |
| `engine/source/mpi/output/spmd_collect_nlocal.F` | MPI gather of non-local data for output |
| `engine/source/mpi/spmd_exch_sub.F` | `SPMD_SUB_BOUNDARIES` (builds MPI non-local comm tables, called once at startup from `resol_init.F`); `SPMD_EXCH_SUB_PON` (exchanges `FSKY`/`STSKY` each step, PARITH/ON); `SPMD_EXCH_SUB_POFF` (exchanges `FNL`/`STIFNL` each step, PARITH/OFF) |
| `starter/source/restart/ddsplit/write_nloc_struct.F` | Domain-decomposition restart output |

---

## 7. Known Limitations / Open Points

### Status of the node-splitting + non-local integration

| Scenario | Status |
|---|---|
| Single domain (`NSPMD = 1`) | ✅ Fully functional — all MPI-gated calls are skipped |
| Multi-domain, PARITH/OFF | ✅ Boundary tables are rebuilt after split |
| Multi-domain, PARITH/ON | ✅ Boundary tables are rebuilt after split |

### Individual issues

1. **Mass splitting is 50/50.**  `detach_node_nloc` splits the non-local mass
   equally between the parent and the new node.  A more physically consistent
   approach would weight the split by the volume fraction of elements
   remaining attached to each node after the split.

2. **`CNE` array is never extended.**  `CNE` (element connectivity in skyline
   format) is always kept at size 0.  If any future code path reads `CNE` for
   the non-local nodes added by splitting, it will access out-of-bounds memory.

3. **Only quad-shell elements are split.**  `detach_node_nloc` handles the
   `IADC` back-pointer update for quad shells only.  Support for triangle
   shells (`IADTG`) and solids (`IADS`) needs to be added when node splitting
   is extended to those element types.

4. **Crack direction is not yet derived from `UNL`.**  The current
   `nloc_shell_detach` implementation uses `UNL` to trigger splitting, but it
   still selects the corner to detach from shell geometry rather than from a
   gradient of `UNL`.  Section 5 above describes how to use `∇UNL` instead.
