# Connect Failure Criterion (`engine/source/materials/fail/connect/`)

Failure criterion for connector (spring/beam) elements: maximum force or displacement threshold with optional progressive degradation.

## Key Files

| File | Role |
|------|------|
| `fail_connect.F` | Connector failure: force or displacement limit for spring/joint elements |

## Criterion

For spring and connector elements, failure is assessed against maximum allowable:
- Axial force `|F| ≥ F_max`
- Transverse force `|F_t| ≥ F_t_max`
- Resultant displacement `|u| ≥ u_max`
- Combined interaction: `(F/F_max)^a + (F_t/F_t_max)^b ≥ 1`

Upon failure the connector is deleted or stiffness is set to zero. Used for spot weld, rivet, and adhesive connector representations where macro-level force-displacement testing data is available.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/elements/spring/README.md` — spring element formulations
- `engine/source/elements/rivet/README.md` — rivet connector
