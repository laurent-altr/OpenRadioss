# Rigid Walls (`starter/source/constraints/general/rwall/`)

Reads /RWALL definitions for geometric rigid-wall obstacles (plane, sphere, cylinder, Lagrange-multiplier variant).

## Key Files

| File | Role |
|------|------|
| `read_rwall.F` | Main rigid wall reader: dispatch by geometry type |
| `hm_read_rwall_plane.F` | Parse planar rigid wall |
| `hm_read_rwall_cyl.F` | Parse cylindrical rigid wall |
| `hm_read_rwall_spher.F` | Parse spherical rigid wall |
| `hm_read_rwall_paral.F` | Parse parallelogram rigid wall |
| `hm_read_rwall_lagmul.F` | Parse Lagrange multiplier rigid wall variant |
| `hm_read_rwall_therm.F` | Parse thermally active rigid wall |
| `init_rwall_penalty.F90` | Compute initial penalty stiffness for rigid wall |
| `split_rwall.F90` | Distribute rigid wall node candidates across SPMD domains |

## Description

Rigid walls are geometric primitives (infinite plane, sphere, cylinder) that reflect or stop impacting slave nodes via penalty or Lagrange multiplier contact. `init_rwall_penalty.F90` estimates the penalty stiffness from the adjacent element stiffness and gap. Distinct from /INTER/TYPE10 in that RWALLs use a simpler search and are often used for symmetry planes.

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `starter/source/interfaces/int10/README.md` — TYPE10 rigid wall interface
