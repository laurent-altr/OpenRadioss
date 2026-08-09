# Damping Control (`starter/source/general_controls/damping/`)

Reads global damping parameters: Rayleigh (mass/stiffness proportional) and nodal velocity damping.

## Key Files

| File | Role |
|------|------|
| `hm_read_damp.F` | Parse /DAMP: Rayleigh α/β coefficients, frequency-range damping |
| `dampdtnoda.F` | Compute damped nodal time-step contribution |
| `damping_range_compute_param.F90` | Compute Rayleigh α/β from frequency range and damping ratio |
| `damping_range_init.F90` | Initialise frequency-range damping data structures |
| `damping_rby_spmdset.F90` | Set SPMD parallel flags for rigid body damping |
| `read_engine_dtmin.F` | Read engine minimum time step (for damping stability check) |

## Description

`hm_read_damp.F` reads `/DAMP` and the extended `/DAMP/FRANGE` form, which specifies target frequencies and damping ratio `ξ`. `damping_range_compute_param.F90` converts `(f1, f2, ξ)` to Rayleigh coefficients via:

```
α = 2ξ·ω1·ω2 / (ω1+ω2)
β = 2ξ / (ω1+ω2)
```

## Related Documentation

- `starter/source/general_controls/README.md` — parent directory
- `engine/source/assembly/README.md` — damping force assembly at runtime
