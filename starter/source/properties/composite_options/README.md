# Composite Options (`starter/source/properties/composite_options/`)

Reads composite laminate definitions for shell and solid elements.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `stack/` | `/STACK` ply stacking sequence definition |
| `drape/` | Drape mapping from forming simulation to structural model |

## Stack (`stack/`)

| File | Role |
|------|------|
| `lecstack_ply.F` | Read `/STACK` keyword: ply sequence (material, thickness, orientation angle) |
| `preplyxfem.F` | Prepare ply data for XFEM crack propagation through composite layers |

A `/STACK` defines an ordered sequence of plies:
```
/STACK
PLY_ID   MAT_ID   THICKNESS   ANGLE
1        5        0.25        0.0
2        5        0.25        90.0
3        5        0.25        45.0
4        5        0.25        -45.0
```

The ply stack is referenced by TYPE10 (composite shell) or TYPE43 (composite solid) properties.

## Drape (`drape/`)

| File | Role |
|------|------|
| `hm_read_drape.F` | HM binary reader for drape data from forming simulation |
| `shellthk_upd.F` | Update shell thickness from drape thinning map |

Draping maps the fibre orientations and thickness changes from a forming/draping simulation onto the structural FE model. This accounts for:
- Fibre angle changes due to draping (shear)
- Thickness reduction due to stretching
- Wrinkling (local compressive regions)

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `starter/source/stack/README.md` — top-level `/STACK` reader
