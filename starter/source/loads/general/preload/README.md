# Starter Preload (`starter/source/loads/general/preload/`)

Reads `/PRELOAD` (initial stress / pretension) input: applies an initial stress state to elements before the simulation begins.

## Key Files

| File | Role |
|------|------|
| `hm_read_preload.F` | Read `/PRELOAD` card: element set, initial stress tensor |
| `hm_pre_read_preload.F` | Pre-read pass for array sizing |
| `hm_read_preload_axial.F90` | Read axial preload for beam/truss elements |

## Description

`/PRELOAD` sets a non-zero initial stress state in elements at t = 0, used for:
- Bolt preload (axial pretension in beam elements)
- Residual stress from manufacturing (initial stress tensor in solids/shells)
- Pre-pressurised structures

`hm_read_preload_axial.F90` handles the common bolt-preload case where only an axial force is specified and the starter computes the corresponding axial stress from the element cross-section area.

## Related Documentation

- `starter/source/loads/general/README.md` — parent directory
- `starter/source/elements/initia/README.md` — initial state more broadly
