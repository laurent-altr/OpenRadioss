# Rivet Property (`starter/source/properties/rivet/`)

Reads rivet connector properties (`/PROP/TYPE20`, `/PROP/TYPE21`, `/PROP/TYPE22`).

## Key Files

| File | Role |
|------|------|
| `hm_read_prop20.F` | Read TYPE20 rivet: spot-weld type connector |
| `hm_read_prop21.F` | Read TYPE21 rivet: general rivet with head geometry |
| `hm_read_prop22.F` | Read TYPE22 rivet: clinch rivet / self-piercing rivet (SPR) |

## Description

Rivet properties define the geometric and material parameters for point connectors between shell structures:
- Rivet diameter, head radius, thickness
- Reference to material law for the rivet shank
- Failure criterion reference

Used with the `/RIVET` element type and combined with failure criteria (e.g., connector fail criterion) for spot-weld and SPR simulation in body-in-white crash models.

## Related Documentation

- `starter/source/properties/README.md` — parent properties directory
- `engine/source/elements/rivet/README.md` — rivet element in engine
