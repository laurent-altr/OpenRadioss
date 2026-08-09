# Engine Ply Stack Access (`engine/source/properties/composite_options/stack/`)

Engine-side ply stack data access routines used during element integration.

## Description

The ply stack parameters (fiber angle, thickness, material ID per ply) are loaded at engine startup from the restart file and stored in `ply_param_mod`. During each step, the shell element integration routines (`engine/source/elements/shell/`, `engine/source/elements/sh3n/`) look up the ply parameters for the current element to:
- Rotate the constitutive tensor from ply material axes to element local axes
- Apply the correct material law for each ply
- Compute the total laminated (ABD) stiffness

This directory contains any engine-specific ply access routines beyond what is in `common_source/modules/mat_elem/`.

## Related Documentation

- `engine/source/properties/composite_options/README.md` — parent directory
- `starter/source/properties/composite_options/stack/README.md` — ply definition
- `common_source/modules/mat_elem/README.md` — ply_param_mod
