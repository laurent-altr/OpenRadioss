# TYPE17 Interface — Hertz Contact (`starter/source/interfaces/int17/`)

Starter reader for /INTER/TYPE17: Hertz contact (sphere-to-sphere, sphere-to-plane) with analytical contact force law.

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_hertz_type17.F` | Parse Hertz contact geometry parameters |
| `hm_read_inter_lagmul_type17.F` | Parse Lagrange multiplier options for TYPE17 |

## Description

TYPE17 implements Hertz elastic contact between spherical or cylindrical bodies. The contact force follows `F = k·δ^(3/2)` (Hertz law) where `k` depends on the elastic moduli and radii of curvature. Used for granular material simulations and discrete element method (DEM) applications.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
