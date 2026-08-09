# LAW16 — Gray Fabric / Woven Composite (`engine/source/materials/mat/mat016/`)

Textile/woven-fabric material with independent warp and weft behaviour;
tension-only fibers, nonlinear shear, and optional compressive buckling.

## Key Files

| File | Role |
|------|------|
| `m16law.F` | Main constitutive update: decoupled warp/weft + shear |
| `gray10.F` / `gray20.F` / `gray21.F` / `gray30.F` | Gray-fabric sub-models (different yarn configurations) |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
