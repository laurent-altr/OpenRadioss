# Ladeveze Damage Model (`engine/source/materials/fail/ladeveze/`)

Intralaminar continuum damage mechanics model for unidirectional composites (Ladeveze & Le Dantec 1992): independent damage variables for fiber, matrix, and shear modes.

## Key Files

| File | Role |
|------|------|
| `fail_ladeveze.F` | Ladeveze damage evolution and stiffness degradation |

## Model

Three scalar damage variables `(d_f, d_m, d_s)` evolve independently:

- `d_f` (fiber damage): driven by fiber-direction strain; irreversible, sudden failure
- `d_m` (matrix cracking): driven by transverse tensile strain; progressive softening
- `d_s` (shear damage): driven by shear strain; coupled with `d_m` via damage surface

Effective stiffness:
```
E_11_eff = (1 − d_f) E_11
E_22_eff = (1 − d_m) E_22
G_12_eff = (1 − d_s) G_12
```

Damage thresholds evolve on a thermodynamic damage surface. This is a CDM model (no element deletion by default) allowing post-peak response simulation for composite impact.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/puck/README.md` — Puck (fracture-plane model)
- `engine/source/materials/fail/lemaitre/README.md` — Lemaitre isotropic CDM
