# LAW15 — Elastic-Plastic Bi-Phase Composite (`engine/source/materials/mat/mat015/`)

Two-phase composite material: elastic-plastic matrix with elastic fiber
reinforcement. Homogenisation is performed via a Voigt or Reuss scheme.

## Key Files

| File | Role |
|------|------|
| `m15cplrc.F` | Material coupler for shell/composite layers |
| `m15crak.F` | Crack initiation and progression tracking |
| `sigeps15c.F` | Shell stress-update wrapper |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
