# LAW35 — Elastic-Plastic (Modified Zerilli-Armstrong) (`engine/source/materials/mat/mat035/`)

Physically-based elastic-plastic model using Zerilli-Armstrong
thermally-activated flow stress: thermal + athermal stress components.

## Key Files

| File | Role |
|------|------|
| `sigeps35.F` | Main stress update: Zerilli-Armstrong rate + temperature dependence |
| `sigeps35c.F` | Shell variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
