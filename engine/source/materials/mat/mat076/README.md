# LAW76 — Anisotropic Plastic (Barlat Yld2004) (`engine/source/materials/mat/mat076/`)

Elastic-plastic material using the Barlat Yld2004-18p anisotropic yield
criterion; supports associated and non-associated flow rules.

## Key Files

| File | Role |
|------|------|
| `sigeps76.F` | Main stress update: Yld2004 yield surface + plastic return |
| `sigeps76c.F` | Shell variant |
| `asso_plas76.F` | Associated flow rule variant |
| `asso_qplas76c.F` | Associated quadratic plastic shell variant |
| `no_asso_lplas76c.F` | Non-associated linear plastic shell variant |
| `no_asso_plas76.F` | Non-associated solid variant |
| `no_asso_qplas76c.F` | Non-associated quadratic shell variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
