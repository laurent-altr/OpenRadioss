# LAW24 — Progressive Composite Failure (Continuum Damage) (`engine/source/materials/mat/mat024/`)

Fabric/composite material with ply-based continuum damage mechanics:
fiber tension/compression and matrix shear damage driven by energy release rates.

## Key Files

| File | Role |
|------|------|
| `m24law.F` | Main stress update: damage-based stiffness degradation |
| `dama24.F` / `rdam24.F` / `udam24.F` | Damage variable evolution (loading / residual / ultimate) |
| `plas24.F` / `plas24b.F` | Plastic strain integration variants |
| `elas24.F` | Elastic predictor |
| `crit24.F` | Failure criterion evaluation |
| `conc24.F` | Concentrated damage localisation |
| `aglo24.F` / `gloa24.F` | Global-to-local / local-to-global transforms |
| `carm24.F` | Carman-Kozeny flow model for resin |
| `pri224.F` / `pri324.F` | Principal-stress 2D / 3D helpers |
| `rotloc.F` | Local-frame rotation |
| `m24dmax.F` / `m24anim.F` | Max-damage tracking / animation output |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
