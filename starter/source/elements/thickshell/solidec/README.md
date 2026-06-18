# TYPE1 Thick-Shell Initialisation (`starter/source/elements/thickshell/solidec/`)

Starter initialisation for TYPE1 8-node solid-shell elements.

## Key Files

| File | Role |
|------|------|
| `scinit3.F` | Main TYPE1 thick-shell initialisation |
| `sccoor3.F` | Compute thick-shell coordinate system |
| `scderi3.F` | Compute shape function derivatives |
| `scdtchk3.F` | Check thick-shell element time-step |
| `scmorth3.F` | Set material orthogonalisation frame |
| `scortho3.F` | Compute orthotropic material axes |
| `scortho31.F` | Alternate orthotropic axis computation |

## Related Documentation

- `starter/source/elements/thickshell/README.md` — parent directory
- `engine/source/elements/thickshell/solidec/README.md` — engine TYPE1 integration
