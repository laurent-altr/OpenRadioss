# Spring Properties (`starter/source/properties/spring/`)

Reads spring and damper element property types.

## Key Files

| File | Property | Description |
|------|----------|-------------|
| `hm_read_prop04.F` | TYPE4 | Nonlinear spring (force-displacement function) |
| `hm_read_prop05.F` | TYPE5 | Spring-damper (linear K + linear C) |
| `hm_read_prop08.F` | TYPE8 | General elasto-plastic spring |
| `hm_read_prop12.F` | TYPE12 | General spring with mass, failure, buckling |
| `hm_read_prop13.F` | TYPE13 | Pulley/rope spring |
| `hm_read_prop23.F` | TYPE23 | Nonlinear spring TYPE23 |
| `hm_read_prop25.F` | TYPE25 | Multi-directional spring |
| `hm_read_prop26.F` | TYPE26 | Spring with hysteresis |
| `hm_read_prop27.F` | TYPE27 | Connector spring |
| `hm_read_prop32.F` | TYPE32 | Spot weld spring (TYPE19-based) |

## Joint Properties (TYPE33)

| File | Joint Type |
|------|-----------|
| `hm_read_prop33.F` | Generic TYPE33 |
| `hm_read_prop33_rev_jnt.F` | Revolute joint |
| `hm_read_prop33_cyl_jnt.F` | Cylindrical joint |
| `hm_read_prop33_sph_jnt.F` | Spherical (ball-and-socket) joint |
| `hm_read_prop33_plan_jnt.F` | Planar joint |
| `hm_read_prop33_trans_jnt.F` | Translational joint |
| `hm_read_prop33_fix_jnt.F` | Fixed (rigid) joint |
| `hm_read_prop33_free_jnt.F` | Free joint |
| `hm_read_prop33_univ_jnt.F` | Universal joint |

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `engine/source/elements/joint/README.md` — joint force computation
