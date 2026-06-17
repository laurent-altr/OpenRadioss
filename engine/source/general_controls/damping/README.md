# Frequency-Range Damping (`engine/source/general_controls/damping/`)

Implements frequency-range damping (`/DAMP`) — viscous damping applied to specific frequency bands.

## Key Files

| File | Role |
|------|------|
| `damping_range_shell.F90` | Apply frequency-range damping to shell elements |
| `damping_range_shell_mom.F90` | Damping applied to shell moments (bending) |
| `damping_range_solid.F90` | Apply frequency-range damping to solid elements |
| `static.F` | Static damping: apply constant viscous damping coefficient |

## Frequency-Range Damping

`/DAMP` targets a specific frequency range `[f_min, f_max]` and damps modes in that range. This is more selective than Rayleigh damping (which damps all modes with quadratic frequency dependence):

```
D(f) = {  C_damp    if f_min ≤ f ≤ f_max
        {  0         otherwise
```

This is useful for:
- Damping high-frequency spurious oscillations without affecting low-frequency structural response
- Modal selective damping in quasi-static implicit analyses
- Vibration isolation modelling

The damping is applied in the element force computation as an additional dissipative force proportional to element-level velocity differences.

## Related Documentation

- `engine/source/general_controls/README.md` — parent directory
- `engine/source/time_step/README.md` — Rayleigh damping in time step context
- `engine/source/assembly/README.md` — global damping assembly
