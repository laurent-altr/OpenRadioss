# LAW96 — Anisotropic Hyperelastic (Holzapfel-Ogden) (`engine/source/materials/mat/mat096/`)

Anisotropic hyperelastic material for soft biological tissue: isotropic
matrix plus two fiber-family strain-energy terms (Holzapfel-Gasser-Ogden).
W = W_iso(Ī₁) + Σₖ Wₖ_aniso(Ī₄ₖ, Ī₆ₖ).

## Key Files

| File | Role |
|------|------|
| `sigeps96.F` | Main Cauchy stress from isotropic + fiber invariants |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
