# Energy Failure Criterion (`engine/source/materials/fail/energy/`)

Failure criterion based on accumulated specific strain energy for isotropic materials.

## Key Files

| File | Role |
|------|------|
| `fail_energy_c.F` | Energy criterion for solid elements |
| `fail_energy_s.F` | Energy criterion for shell elements |
| `fail_energy_b.F` | Energy criterion for beam elements |
| `fail_energy_ib.F` | Energy criterion for implicit beam elements |

## Criterion

Failure occurs when the cumulative specific internal energy exceeds the threshold `W_max`:

```
W = ∫₀ᵗ σ:ε̇ dt ≥ W_max
```

`W_max` is typically the area under the stress-strain curve to fracture. This criterion is path-dependent and accounts for loading history, including loading/unloading cycles. Widely used for foams and elastomers where rupture energy is the primary characterisation.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/orthenerg/README.md` — orthotropic energy
