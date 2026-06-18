# LAW100 — Hyperelastic (Neo-Hookean / Arruda-Boyce Variants) (`engine/source/materials/mat/mat100/`)

Family of isotropic hyperelastic models for rubber: Neo-Hookean, polynomial
(Rivlin), and Arruda-Boyce (8-chain); all with optional Prony visco-elastic
correction and compressibility term.

## Key Files

| File | Role |
|------|------|
| `neo_hook_t.F` | Neo-Hookean thermal variant |
| `sigaboyce.F` | Arruda-Boyce 8-chain model: Σ Cₙ(Ī₁^n − 3^n) |
| `sigpoly.F` | Polynomial Rivlin series variant |
| `calcmatb.F` | Material stiffness (Bmatrix) computation for implicit |
| `viscbb.F` | Visco-elastic back-stress (Bergström-Boyce) |
| `viscpower.F` | Power-law visco-elastic flow |
| `viscsinh.F` | Sinh-law visco-elastic flow |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
