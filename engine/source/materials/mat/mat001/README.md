# LAW1 — Elastic (`engine/source/materials/mat/mat001/`)

Linear isotropic elastic material (Hooke's law). Supports small-strain
and large-strain (incremental/total) formulations, and full-integration
shell (QEPH/BT).

## Key Files

| File | Role |
|------|------|
| `m1law.F` | Main constitutive update: stress from strain increment via Lamé constants |
| `m1lawi.F` | Incompressible elastic variant |
| `m1lawtot.F` | Total-strain formulation |
| `m1law8.F` | 8-node solid variant |
| `m1lawp.F` | Plane-stress projection |
| `m1lawt.F` | Thermal coupling |
| `m1ismstr10_pon.F` / `m1ismstr11.F` | ISMSTR=10/11 co-rotational variants |
| `sigeps01c.F` / `sigeps01g.F` | Shell / SPH wrappers |
| `princ_u.F` / `princ_u1.F` | Principal-stress rotation utilities |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
- `engine/source/materials/mat_share/README.md` — `mmain.F90` dispatch
