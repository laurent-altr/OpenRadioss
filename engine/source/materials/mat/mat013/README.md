# LAW13 — Elastic-Plastic with Reinforcement (`engine/source/materials/mat/mat013/`)

Elastic-plastic material model for reinforced concrete: isotropic matrix
with embedded directional reinforcement fibers.

## Key Files

| File | Role |
|------|------|
| `m13law.F` | Main constitutive update: matrix + reinforcement contributions |
| `condrmat.F` | Directional reinforcement stiffness assembly |
| `rmatacce.F` | Reinforcement acceleration update |
| `rmatforp.F` | Reinforcement force projection |
| `rmatpon.F` | Reinforcement normal-force contribution |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
