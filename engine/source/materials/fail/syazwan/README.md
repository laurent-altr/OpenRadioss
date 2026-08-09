# Syazwan Failure Criterion (`engine/source/materials/fail/syazwan/`)

Failure criterion for natural rubber and rubber-like materials under large deformation (Syazwan formulation).

## Key Files

| File | Role |
|------|------|
| `fail_syazwan_c.F` | Syazwan criterion for solid elements |
| `fail_syazwan_s.F` | Syazwan criterion for shell elements |

## Criterion

The Syazwan criterion uses stretch-based failure in the principal stretch space for hyperelastic materials:

```
λ_max ≥ λ_crit    (maximum principal stretch at failure)
```

with optional energy density threshold as a secondary check:

```
W = ∫ σ:dε ≥ W_crit
```

The stretch-based formulation is more natural for hyperelastic materials than strain-based criteria because it is frame-invariant under large rotations. Used with rubber-like material laws (LAW042, LAW069) for seals, o-rings, and elastomeric bumpers.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/mullins_or/README.md` — Mullins effect for cyclically loaded rubber
- `engine/source/materials/fail/energy/README.md` — strain energy criterion
