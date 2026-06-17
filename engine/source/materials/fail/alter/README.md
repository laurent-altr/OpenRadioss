# Alter Failure Criterion (`engine/source/materials/fail/alter/`)

Alternative criteria set (Alter = alternate): custom/experimental failure models for specialist applications.

## Key Files

| File | Role |
|------|------|
| `fail_brokmann.F` | Brokmann criterion: combined stress-triaxiality and Lode angle for polymers |
| `fail_wind_frwave.F` | Wind-fracture wave criterion for glass/brittle materials |
| `fail_wind_frwave_init.F` | Initialise wind-fracture wave data |
| `fail_wind_xfem.F` | Wind criterion driving XFEM crack for brittle fracture |

## Description

The `alter` directory groups specialist fracture models that do not fit the standard categories:

- **Brokmann**: A polymer fracture criterion combining triaxiality and Lode parameter, calibrated from multi-axial polymer tests (similar to Hosford-Coulomb but with polymer-specific calibration data)
- **Wind/frwave**: Fracture wave model for windshield glass: a brittle fragmentation front propagates from the impact point with prescribed velocity and angular spread, converting elements to fragmented glass material state
- **Wind/XFEM**: Drives XFEM level-set from the Wind fracture criterion for explicit crack-network simulation in laminated safety glass

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/failwave/README.md` — generic failure wave propagation
- `engine/source/elements/xfem/README.md` — XFEM crack framework
