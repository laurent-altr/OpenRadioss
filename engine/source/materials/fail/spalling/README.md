# Spalling Failure Criterion (`engine/source/materials/fail/spalling/`)

Pressure-threshold spall criterion: element fails when compressive hydrostatic pressure falls below a negative (tensile) limit.

## Key Files

| File | Role |
|------|------|
| `fail_spalling_s.F90` | Spalling failure check for shell elements |

## Criterion

Spall failure triggered when:

```
p < −p_spall
```

where `p = −(σ_11 + σ_22 + σ_33)/3` is the hydrostatic pressure (positive in compression). `p_spall` is the material's spall strength (tensile hydrostatic stress at which voids nucleate and coalesce).

This is the simplest spall model, appropriate for brittle materials (ceramics, concrete, rock) where fracture is nearly instantaneous once tensile pressure exceeds the dynamic tensile strength. For rate-dependent spall use the Tuler-Butcher or Wilkins criteria.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/tuler_butcher/README.md` — cumulative spall model
- `engine/source/materials/fail/wilkins/README.md` — Wilkins dynamic fracture
