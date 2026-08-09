# Initial Evolution Failure Criterion (`engine/source/materials/fail/inievo/`)

Failure criterion with an initial damage evolution phase: damage initiates at a threshold and evolves along a softening law before element deletion.

## Key Files

| File | Role |
|------|------|
| `fail_inievo_c.F` | Initial-evolution failure for solid elements |
| `fail_inievo_s.F` | Initial-evolution failure for shell elements |
| `fail_inievo_b.F90` | Initial-evolution failure for beam elements |
| `fail_inievo_ib.F90` | Initial-evolution failure for implicit beam elements |

## Model

Two-phase damage model:
1. **Initiation**: damage starts when a stress/strain indicator reaches threshold `f_ini`
2. **Evolution**: damage grows from 0 to 1 following a user-defined law (linear, exponential, or tabulated softening) until element deletion

```
D = 0           if f < f_ini
D = g(f − f_ini)   if f ≥ f_ini   (progressive)
```

This approach avoids abrupt element deletion and produces a more mesh-objective response through the fracture energy parameter (energy dissipated per unit area of crack surface). Suitable for adhesives, polymers, and crashbox-type absorbers.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/lemaitre/README.md` — coupled CDM with softening
