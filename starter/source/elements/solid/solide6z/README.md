# Wedge Solid Initialisation (`starter/source/elements/solid/solide6z/`)

Starter initialisation for 6-node pentahedral (wedge) solid elements with Z-type integration.

## Key Files

| File | Role |
|------|------|
| `s6zinit3.F90` | Main wedge element initialisation |
| `s6zcoor3.F90` | Compute wedge coordinate system |
| `s6zderi3.F90` | Compute wedge shape function derivatives |
| `s6zjacidp.F90` | Compute wedge Jacobian at integration point |
| `s6zortho3.F90` | Compute wedge orthotropic axes |
| `s6zrcoor3.F90` | Compute wedge reference coordinate system |

## Related Documentation

- `starter/source/elements/solid/README.md` — parent directory
- `engine/source/elements/solid/solide6z/README.md` — engine wedge integration
