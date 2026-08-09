# Fabric Failure Criterion (`engine/source/materials/fail/fabric/`)

Failure criterion for woven fabric materials: independent failure checks in warp and weft directions.

## Key Files

| File | Role |
|------|------|
| `fail_fabric_c.F` | Fabric failure for solid elements |

## Criterion

Woven fabrics behave differently from UD composites due to yarn crimp and orthogonal interlacing. The criterion checks independently:

```
ε_warp ≥ ε_warp_max    (yarn tension in warp direction)
ε_weft ≥ ε_weft_max    (yarn tension in weft direction)
γ_warp_weft ≥ γ_max    (in-plane shear / yarn locking angle)
```

Failure in one direction triggers stiffness reduction only in that direction (progressive failure). Used with airbag fabric laws (LAW034) and structural fabric in crash models.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/airbag/README.md` — airbag fabric simulation context
