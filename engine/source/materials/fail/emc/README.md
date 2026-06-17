# EMC Failure Criterion (`engine/source/materials/fail/emc/`)

Electromagnetic-coupled failure criterion: failure triggered by electromagnetic field intensity in electrically active materials.

## Key Files

| File | Role |
|------|------|
| `fail_emc.F` | EMC failure: coupling electromagnetic field quantities to mechanical failure |

## Description

`fail_emc.F` evaluates a failure indicator based on electromagnetic quantities (current density, Joule heating, or field intensity) computed by the electromagnetic solver. Failure occurs when the coupled EM-induced damage exceeds a threshold.

This criterion is used in simulations of electromagnetic forming, magnetic pulse welding, or electro-explosive device initiation where electromagnetic energy deposition drives material fracture or phase change.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
