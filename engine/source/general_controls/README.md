# General Controls Subsystem

This subsystem handles global simulation control parameters that affect the overall run behaviour but do not belong to a specific physics subsystem.

## Directory Structure

```
general_controls/
├── computation/   — Computation control (noise, random perturbation)
└── damping/       — Global frequency-domain damping filters
```

## Computation Controls (`computation/`)

| File | Keyword | Description |
|------|---------|-------------|
| `noise.F` | `/NOISE` | Apply random geometric perturbations to node positions |
| `lecnoise.F` | `/NOISE` | Read `/NOISE` parameters from restart |
| `initnoise.F` | `/NOISE` | Initialise the random perturbation data |
| `pnoise.F` | `/NOISE` | Parallel (MPI-aware) noise application |

### Noise (`/NOISE`)

Random small perturbations are applied to node positions at the start of a run to break unphysical symmetry in buckling or crushing simulations. Without noise, a perfectly symmetric model may exhibit an unphysically symmetric collapse mode.

## Damping Controls (`damping/`)

| File | Keyword | Description |
|------|---------|-------------|
| `static.F` | `/DAMP/STATIC` | Static (quasi-static) damping — high-frequency energy dissipation |
| `damping_range_shell.F90` | `/DAMP/FREQS` | Frequency-range viscous damping for shell elements |
| `damping_range_shell_mom.F90` | `/DAMP/FREQS` | Frequency-range damping with moment correction |
| `damping_range_solid.F90` | `/DAMP/FREQS` | Frequency-range viscous damping for solid elements |

### Frequency-Range Damping (`/DAMP/FREQS`)

This damping type targets a specific frequency band, adding viscous damping only to modes within the specified range. It is implemented as a viscous stress contribution scaled by the element's current strain rate, filtered to the target frequency band.

Useful for suppressing specific numerical artifacts (e.g. high-frequency contact chatter) without over-damping the physical response.

### Static Damping (`/DAMP/STATIC`)

Applies a velocity-proportional damping that drives the solution toward static equilibrium. Used in quasi-static analyses to damp out inertia effects. The damping coefficient is automatically adjusted to ensure convergence to the static solution.

## Global Run Control

Additional global controls (not in subdirectories here) are handled by the starter's `general_controls/` and stored in the restart file:
- `/RUN` — run time, CPU limit
- `/PRINT` — output frequency
- `/TITLE` — simulation title
- `/UNIT` — unit system

These are read by the starter and are available to the engine through the model data structures.

## Related Documentation

- `engine/source/assembly/README.md` — Rayleigh damping (separate, element-level)
- `engine/source/time_step/README.md` — DT control (separate from run control)
