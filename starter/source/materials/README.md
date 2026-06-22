# Starter Materials Subsystem

This subsystem reads and validates all material and failure keyword definitions, then builds the material property tables (`PM`, `IPM`) written to the restart file.

## Role

The starter's `materials/` directory is the **parsing and initialisation** counterpart to the engine's `engine/source/materials/` directory. For each material law, the starter:
1. Reads keyword parameters from the input deck
2. Validates parameter ranges (e.g. density > 0, Young's modulus > 0)
3. Evaluates any initial derived quantities (thermal coefficients, yield stress, etc.)
4. Packs parameters into `PM` (real) and `IPM` (integer) arrays
5. Writes to the restart file

## Directory Structure

```
materials/
├── mat/           — Per-law material keyword parsers (mat001/–mat190/)
├── mat_share/     — Shared parsing utilities (common to multiple laws)
├── fail/          — Failure criterion keyword parsers
├── ale/           — ALE/Euler material setup (EOS coupling)
├── eos/           — EOS parameter reading
├── therm/         — Thermal material parameter reading
└── nonlocal/      — Non-local regularisation parameter reading
```

## Material Laws (`mat/`)

Each `mat<NNN>/` subdirectory contains the keyword reader for `/MAT/LAW<NNN>`. The files are named `lec<NNN>.F` or `read_mat<NNN>.F`.

There are over 110 material law subdirectories mirroring the engine's `materials/mat/` structure.

## Failure Criteria (`fail/`)

Each failure criterion subdirectory reads the corresponding `/FAIL/<name>` keyword. Parameters are validated and packed into the material's IPM/PM arrays alongside the constitutive law parameters.

## EOS Reading (`eos/`)

Reads `/EOS/<type>` keyword parameters and packs them for the EOS models in `common_source/eos/`.

## Shared Reading (`mat_share/`)

Helper routines used by multiple material laws:
- `read_material_models.F` — dispatch reader by law number
- `material_is_high_explosive.F90` — detect high-explosive materials (affects ALE setup)
- `globmat.F` — global material array management

## Non-Local Regularisation (`nonlocal/`)

Reads `/MAT/LAW<N>/NONLOCAL` parameters that enable non-local averaging of damage variables to prevent mesh dependency in softening materials.

## Thermal Materials (`therm/`)

Reads thermal material parameters for thermo-mechanical coupled analyses (conductivity, specific heat, expansion coefficient).

## Related Documentation

- `engine/source/materials/README.md` — runtime material law evaluation
- `common_source/eos/README.md` — EOS models shared between starter and engine
