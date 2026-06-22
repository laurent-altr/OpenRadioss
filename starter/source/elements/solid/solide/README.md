# 8-Node Solid Initialisation (`starter/source/elements/solid/solide/`)

Starter initialisation for the standard 8-node hexahedral solid element (one-point QEPH integration).

## Key Files

| File | Role |
|------|------|
| `sinit3.F` | Main solid element initialisation |
| `smass3.F` | Compute 8-node solid nodal mass |
| `scoor3.F` | Compute element coordinate system |
| `sderi3.F` | Compute shape function derivatives at integration point |
| `sdefo3.F` | Compute deformation gradient |
| `sdlen3.F` | Compute element characteristic length for DT |
| `slen.F` | Element edge length computation |
| `sgrhead.F` | Write solid group header to restart |
| `sgrtails.F` | Write solid group tail data to restart |
| `smorth3.F` | Set material orthogonalisation frame |
| `sortho3.F` | Compute orthotropic material axes |
| `srho3.F` | Compute element density |
| `sbulk3.F` | Compute bulk modulus for element |
| `sjacidp.F` | Jacobian computation at integration point |
| `sveok3.F` | Element volume check (negative Jacobian detection) |
| `checksvolume.F` | Check solid element volume for validity |
| `failini.F` | Initialise failure state |
| `srefsta3.F` | Set reference state for stress initialisation |
| `srepiso3.F` | Isotropic stress representation initialisation |
| `sreploc3.F` | Local stress representation initialisation |
| `srcoor3.F` | Reference coordinate system |
| `srorth3.F` | Reference orthogonalisation |
| `srrota3.F` | Reference rotation |
| `userin3.F` | User material state initialisation |
| `inimom_fvm.F` | FVM momentum initialisation for ALE solid |
| `mod_close.F` | Moduli closure for EOS-driven solids |

## Related Documentation

- `starter/source/elements/solid/README.md` — parent directory
- `engine/source/elements/solid/solide/README.md` — engine solid integration
