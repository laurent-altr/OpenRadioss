# LAW14 — Elastic-Plastic Orthotropic with Tabulated Hardening (`engine/source/materials/mat/mat014/`)

Orthotropic elastic-plastic material for composites and wood, with
direction-dependent tabulated hardening curves and progressive softening.

## Key Files

| File | Role |
|------|------|
| `m14law.F` | Main constitutive update: orthotropic yield + table interpolation |
| `m14ama.F` | Anisotropic material stiffness assembly |
| `m14ftg.F` | Fatigue / damage accumulation |
| `m14gtf.F` | Global-to-fiber coordinate transform |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
