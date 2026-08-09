# HC-DSSE Failure Criterion (`engine/source/materials/fail/hc_dsse/`)

Hosford-Coulomb Ductile Strain-to-Shear-Failure criterion: an anisotropic ductile fracture locus for sheet metals accounting for shear-dominated failure.

## Key Files

| File | Role |
|------|------|
| `fail_hc_dsse_c.F` | HC-DSSE failure check for solid elements |

## Criterion

The HC-DSSE criterion extends the Hosford-Coulomb fracture locus with a shear-stress correction term to better capture shear-band fracture:

```
ε_f(η, θ̄, γ̇) = f_HC(η, θ̄) × g_shear(γ̇)
```

where `f_HC` is the Hosford-Coulomb base locus and `g_shear` is a shear-rate amplification factor. Calibrated from butterfly specimens under combined tension/shear loading. Primarily used for advanced high-strength steels (AHSS/UHSS) where shear fracture during edge-stretch forming is a critical failure mode.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/wierzbicki/README.md` — base Hosford-Coulomb locus
- `engine/source/materials/fail/tabulated/README.md` — tabulated fracture locus
