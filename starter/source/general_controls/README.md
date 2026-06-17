# Starter General Controls Subsystem

This subsystem reads the global control keywords that govern the overall simulation setup: run parameters, output frequencies, engine options, unit systems, and ALE/CFD configuration.

## Directory Structure

```
general_controls/
├── ale_cfd/       — ALE / CFD global option reading
├── ale_grid/      — ALE grid parameter reading
├── computation/   — Computation control parameters (/NOISE, /RANDOM, …)
├── damping/       — Global damping parameters
├── default_values/ — Default value initialisation for optional parameters
├── engine/        — Engine-level control keywords readable from the starter
├── inputoutput/   — I/O control (/PRINT, /ANIM/, /TH/, /H3D/, …)
```

## Key Keyword Families

### Run Control
- `/RUN` — simulation title, end time, CPU limit
- `/UNIT` — unit system (SI, CGS, mm/ms/g, custom)
- `/TITLE` — simulation title

### Output Control (`inputoutput/`)
- `/PRINT` — output frequency for `.out` file entries
- `/ANIM/` — animation output frequency and result selection
- `/TH/` — time history output setup (entity selection + result types)
- `/H3D/` — H3D output frequency and result selection

### ALE/CFD Controls (`ale_cfd/`, `ale_grid/`)
- `/ALE/` — ALE formulation options
- `/ALE/GRID/` — mesh motion strategy
- `/CFD/` — CFD solver parameters

### Damping (`damping/`)
- `/DAMP` — global Rayleigh damping
- `/DAMP/FREQS` — frequency-range damping

### Noise / Perturbation (`computation/`)
- `/NOISE` — random geometric perturbation

### Engine Controls (`engine/`)
Some `/DT/` and solver-control keywords are parsed in the starter and stored in the restart file for the engine to read:
- `/DT/NODA`, `/DT/AMS`, `/DT/BRICK`
- `/IMPL/SOLVER`, `/IMPL/MODAL`

## Default Values (`default_values/`)

Sets default values for all optional parameters before keyword parsing begins. This ensures that if a keyword is absent from the input deck, a sensible default is used rather than an uninitialised value.

## Related Documentation

- `engine/source/general_controls/README.md` — engine-side computation controls
- `engine/source/output/README.md` — output writers driven by these parameters
- `engine/source/time_step/README.md` — DT controls parsed here
