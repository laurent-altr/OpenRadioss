# NXT Failure Criterion (`engine/source/materials/fail/nxt/`)

Next-generation tabulated failure criterion: extended multi-variable fracture locus for advanced materials.

## Key Files

| File | Role |
|------|------|
| `fail_nxt_c.F` | NXT criterion for solid elements |

## Description

The NXT criterion is an extended tabulated fracture model accepting:
- 3D fracture locus `ε_f(η, θ̄, ε̇)` (triaxiality, Lode angle, strain rate)
- Temperature scaling via additional table axis
- Regularisation length scale for mesh-size independence

Damage accumulates via:
```
D = ∫ dε_p / ε_f(η, θ̄, ε̇, T)
```

NXT is intended as the successor to the tabulated criterion (`fail_tab`) with more physical axes and built-in regularisation. Primarily used for AHSS/UHSS crash simulation requiring both shear and tensile fracture prediction.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/tabulated/README.md` — earlier tabulated criterion
- `engine/source/materials/fail/wierzbicki/README.md` — Hosford-Coulomb locus
