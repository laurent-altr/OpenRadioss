# Boundary Conditions (`engine/source/constraints/general/bcs/`)

Applies displacement and velocity boundary conditions (`/BCS`) each time step.

## Key Files

| File | Role |
|------|------|
| `bcs0.F` | Apply zero-displacement (fixed) BCs — zero velocity for constrained DOF |
| `bcs1.F` | Apply TYPE1 BCs: prescribed velocity via function curve |
| `bcs2.F` | Apply TYPE2 BCs: prescribed displacement via function curve |
| `bcs10.F` | Apply TYPE10 BCs: nodal translational + rotational constraints |
| `bcsn.F` | General BC dispatcher: call bcs0/1/2/10 based on BC type |
| `bcscyc.F` | Cyclic symmetry BCs — link DOF across periodic boundaries |
| `bcsdtth.F` | BC time step (thermal): compute thermal BC contribution |
| `bc_imp0.F` | BC for implicit solver: assemble BC into implicit stiffness |
| `lcbcsf.F` | Load curve BC: evaluate function curve for time-varying BC |
| `ply_bcs.F` | Composite ply boundary conditions |

## BC Types

| Type | Constraint |
|------|-----------|
| 0 | Fixed (zero velocity) — most common |
| 1 | Prescribed velocity = f(t) × scale |
| 2 | Prescribed displacement = f(t) × scale |
| cyclic | V_A = V_B (periodic pair) |

BCs zero out (or set) specific velocity components at constrained nodes after the force-to-acceleration step. This is the "velocity projection" approach — forces are computed normally, then the velocity is corrected to satisfy the BC.

## Related Documentation

- `engine/source/constraints/general/README.md` — parent directory
- `engine/source/boundary_conditions/README.md` — EBCS (absorbing, cyclic surface BCs)
