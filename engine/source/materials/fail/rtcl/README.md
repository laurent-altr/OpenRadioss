# Rice-Tracey / Cockcroft-Latham (RTCL) Criterion (`engine/source/materials/fail/rtcl/`)

Combined ductile fracture criterion: Rice-Tracey void-growth model for tensile triaxiality + Cockcroft-Latham for shear-dominated failure.

## Key Files

| File | Role |
|------|------|
| `fail_rtcl_c.F` | RTCL criterion for solid elements |
| `fail_rtcl_s.F` | RTCL criterion for shell elements |

## Criterion

Two damage indices accumulated independently:

**Rice-Tracey** (void growth, triaxiality-driven):
```
D_RT = ∫ exp(1.5 σ*) dε_p
```

**Cockcroft-Latham** (surface cracking, shear-driven):
```
D_CL = ∫ ⟨σ_max⟩ dε_p
```

Failure when `max(D_RT/D_RT_crit, D_CL/D_CL_crit) ≥ 1`. The combined criterion is effective for metal cutting and high-velocity impact simulations where both void coalescence and shear localisation are active failure mechanisms.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/cockroft_latham/README.md` — pure CL criterion
- `engine/source/materials/fail/wierzbicki/README.md` — extended Hosford-Coulomb locus
