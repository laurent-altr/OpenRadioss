# Friction Models (`starter/source/interfaces/friction/`)

Reads friction model definitions (/FRICTION, /FRICTION/ORTHO) that are referenced by contact interfaces.

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `reader/` | Duplicate/alternate location of friction readers |

## Key Files

| File | Role |
|------|------|
| `hm_read_friction.F` | Parse /FRICTION card: model type, Coulomb coefficient, viscous term |
| `hm_read_friction_models.F` | Parse named friction models and their parameters |
| `hm_read_friction_orientations.F` | Parse orthotropic friction orientation (local axes) |

## Description

Friction models are defined independently of contact interfaces and referenced by friction ID. Supported models include: static/dynamic Coulomb (`μ_s`, `μ_d`), velocity-dependent (`μ(v)`), temperature-dependent (`μ(T)`), Darmstadt model, and orthotropic friction for fabrics and composite surfaces. `hm_read_friction_orientations.F` reads local material axes for orthotropic friction directionality.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher (references friction IDs)
- `engine/source/interfaces/README.md` — runtime friction evaluation
