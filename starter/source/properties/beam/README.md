# Beam Properties (`starter/source/properties/beam/`)

Reads beam element property types.

## Key Files

| File | Property | Description |
|------|----------|-------------|
| `hm_read_prop03.F` | TYPE3 | Euler-Bernoulli / Timoshenko beam with standard cross-section |
| `hm_read_prop18.F` | TYPE18 | Integrated beam with arbitrary cross-section mesh |
| `defbeam_sect_new.F90` | — | Define beam cross-section from geometry (auto-compute I, J, A) |

## Cross-Section Parameters (TYPE3)

TYPE3 beam requires cross-section geometric properties stored in `PM`:
- Area `A`
- Bending moments of inertia `Iy`, `Iz`
- Torsion constant `J`
- Shear correction factors `κy`, `κz`
- Warping constant `Γ` (for thin-walled open sections)

These can be provided directly or computed automatically from a standard profile code (see `starter/source/tools/univ/README.md`).

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `starter/source/tools/univ/README.md` — standard cross-section library
- `engine/source/elements/beam/README.md` — beam force computation
