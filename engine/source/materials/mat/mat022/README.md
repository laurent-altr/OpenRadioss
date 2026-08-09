# LAW22 — Chang-Chang Composite Failure (`engine/source/materials/mat/mat022/`)

Orthotropic elastic material with Chang-Chang progressive failure criteria
for fiber tension/compression, matrix tension/compression, and delamination.

## Key Files

| File | Role |
|------|------|
| `m22law.F` | Main stress update: orthotropic elasticity + failure flag tracking |
| `m22cplr.F` | Layer coupler for laminated shells |
| `sigeps22c.F` / `sigeps22g.F` | Shell / SPH wrappers |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
