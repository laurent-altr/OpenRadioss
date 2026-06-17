# Shared Material Utilities (`starter/source/materials/mat_share/`)

Shared material initialisation routines used by multiple material laws in the starter.

## Key Files

| File | Role |
|------|------|
| `matini.F` | Generic material initialisation: allocate and zero material parameter arrays |
| `cmatini.F` | Composite material initialisation (orthotropic parameters) |
| `cmatini4.F` | Composite initialisation for 4-parameter failure |
| `cnloc_matini.F` | Nonlocal damage model initialisation |
| `mating.F` | Material-to-group assignment (assign which element groups use this material) |
| `mmain.F` | Main material pre-processing: call per-law init, validate parameters |
| `mulaw.F` | Material law type dispatch table: map law number to initialisation routine |

## Material Parameter Arrays

Material parameters are stored in two arrays per material law:
- `PM(NPROPELL, IMAT)` — real parameters (Young's modulus, yield stress, etc.)
- `IPM(NIPROPELL, IMAT)` — integer parameters (flags, curve IDs, table IDs)

`matini.F` allocates these arrays. Each per-law `mat_share` routine fills the relevant `PM`/`IPM` entries.

## Law Dispatch

`mulaw.F` maps the material law number (e.g., 36 for LAW36 Tabulated piecewise-linear) to its specific initialisation subroutine. This is the starter counterpart to the engine's `SIGEPS<NNN>` dispatch.

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `engine/source/materials/README.md` — engine material force computation
