# Puck Failure Criterion (`engine/source/materials/fail/puck/`)

Implements the Puck (1996/1998) physically based failure criteria for fiber-reinforced composites, distinguishing fiber fracture from inter-fiber fracture with fracture plane orientation.

## Key Files

| File | Role |
|------|------|
| `fail_puck_c.F90` | Puck criterion for solid elements |
| `fail_puck_s.F90` | Puck criterion for shell elements |

## Criterion

**Fiber failure** (FF): similar to Hashin fiber modes.

**Inter-fiber failure** (IFF): searches for the critical fracture plane angle `θ_fp` that maximises the Puck stress exposure factor:

```
f_E(θ) = √[(τ_nt/R_A⊥⊥)² + (τ_nl/R_A⊥∥)² + (σ_n/R_A⊥)²]
```

Puck's formulation accounts for the influence of compressive transverse stress on inter-fiber fracture resistance (parabolic fracture curve), making it more accurate than Hashin for compression-dominated failure.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/hashin/README.md` — simpler Hashin criterion
- `engine/source/materials/fail/ladeveze/README.md` — Ladeveze continuum damage
