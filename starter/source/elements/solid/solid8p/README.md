# 8-Point Solid Initialisation (`starter/source/elements/solid/solid8p/`)

Starter initialisation for 8-node hexahedral solid elements with 8-point (full) integration (EAS or reduced hourglass).

## Key Files

| File | Role |
|------|------|
| `basis8.F` | Compute 8-point integration basis functions |
| `basisf.F` | Compute full-integration basis for EAS |
| `sderi3b.F` | Compute shape function derivatives at 8 integration points |
| `sigin3b.F` | Initialise stress state at 8 integration points |
| `smass3b.F` | Compute nodal mass from 8-point integration |

## Related Documentation

- `starter/source/elements/solid/README.md` — parent directory
- `engine/source/elements/solid/solide8/README.md` — engine 8-point solid integration
