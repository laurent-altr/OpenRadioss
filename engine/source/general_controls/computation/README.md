# Noise Control (`engine/source/general_controls/computation/`)

Implements the `/NOISE` keyword — numerical noise injection and random perturbation for mesh symmetry breaking.

## Key Files

| File | Role |
|------|------|
| `noise.F` | Main noise computation: apply random perturbation to nodal velocities |
| `initnoise.F` | Initialise noise parameters: read amplitude, frequency, seed from restart |
| `lecnoise.F` | Engine-side reader for `/NOISE` parameters |
| `pnoise.F` | Parallel noise application (OpenMP version) |

## Purpose of `/NOISE`

In symmetric models, symmetric loading can cause perfectly symmetric response — but physical structures never fail symmetrically due to manufacturing variations. `/NOISE` injects a small random perturbation to break symmetry:
```
v_i ← v_i + ε_noise × rand(-1, 1) × v_max
```
where `ε_noise` is the noise amplitude (typically 10⁻⁶ to 10⁻⁴).

This is important for:
- Buckling simulations (trigger buckling into the correct mode)
- Impact with symmetric geometry (prevent artificial symmetric deformation)
- Crash simulations (trigger realistic asymmetric fold patterns)

## Related Documentation

- `engine/source/general_controls/README.md` — parent general controls directory
