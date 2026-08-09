# LAW36 — Elastic-Plastic with Tabulated Kinematic Hardening (`engine/source/materials/mat/mat036/`)

Elastic-plastic with tabulated strain-rate-dependent kinematic hardening;
supports implicit (iterative) return mapping for stiff rate terms.

## Key Files

| File | Role |
|------|------|
| `sigeps36.F` | Main stress update: tabulated kinematic hardening + radial return |
| `sigeps36c.F` / `sigeps36g.F` | Shell / SPH wrappers |
| `sigeps36pi.F` | Plane-stress incompressible variant |
| `m36iter_imp.F` | Implicit Newton iteration for stiff kinematic terms |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
