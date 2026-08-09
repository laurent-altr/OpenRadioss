# Maximum Strain Failure Criterion (`engine/source/materials/fail/max_strain/`)

Implements element deletion based on maximum principal or engineering strain thresholds.

## Key Files

| File | Role |
|------|------|
| `fail_maxstrain_c.F` | Maximum strain check for solid elements |
| `fail_maxstrain_s.F` | Maximum strain check for shell elements |

## Criterion

Failure occurs when any user-specified strain component exceeds its threshold:

```
|ε_i| ≥ ε_max_i    (for i = 11, 22, 33, 12, 23, 31, or principal)
```

Independent thresholds may be set for tension vs. compression in each direction. This is the simplest criterion and is used when material-level test data is available only in terms of elongation-at-break or shear strain to failure (common for polymers and foams).

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/biquad/README.md` — bi-quadratic strain-space criterion
