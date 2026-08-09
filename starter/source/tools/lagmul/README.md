# Lagrange Multipliers (`starter/source/tools/lagmul/`)

Reads and initialises `/LAGMUL` (Lagrange multiplier) constraint definitions — algebraic constraints enforced exactly without penalty stiffness.

## Key Files

| File | Role |
|------|------|
| `hm_read_lagmul.F` | HM binary reader for `/LAGMUL` keyword |
| `lagm_ini.F` | Main Lagrange multiplier initialisation: build constraint matrix rows |
| `lagm_nhf.F` | Non-homogeneous force term for constrained DOF (prescribed displacement Lagrange) |
| `lgmini_bc.F` | Lagrange multiplier for displacement boundary conditions (`/BCS` enforcement) |
| `lgmini_fxv.F` | Lagrange multiplier for fixed-velocity constraints |
| `lgmini_gj.F` | Lagrange multiplier for glue-joint constraints (tied surfaces) |
| `lgmini_i2.F` | Lagrange multiplier for TYPE2 (tied) interface |
| `lgmini_i7.F` | Lagrange multiplier for TYPE7 (contact) interface |
| `lgmini_mpc.F` | Multi-point constraint (MPC / RBE3-style) Lagrange multiplier |
| `lgmini_rby.F` | Lagrange multiplier for rigid body (RBODY) constraint |
| `lgmini_rwl.F` | Lagrange multiplier for rigid wall constraint |
| `ini_diff.F` | Initialise differential constraint (relative DOF constraint between two nodes) |
| `ini_gear.F` | Initialise gear constraint (rotational coupling with gear ratio) |
| `ini_rack.F` | Initialise rack-and-pinion constraint |

## Lagrange Multipliers vs Penalty

OpenRadioss supports both methods for constraints:
- **Penalty**: adds stiffness to resist violation — approximate, no extra DOF
- **Lagrange multiplier**: exact enforcement — adds one DOF per constraint

The implicit solver uses Lagrange multipliers for exact constraint satisfaction. Explicit runs typically use penalty-based contacts; Lagrange is preferred for joints and rigid body constraints.

## Related Documentation

- `starter/source/tools/README.md` — parent tools directory
- `engine/source/implicit/README.md` — implicit solver uses Lagrange multipliers
- `engine/source/constraints/README.md` — constraint enforcement in engine
