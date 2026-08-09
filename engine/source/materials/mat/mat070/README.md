# LAW70 — Thermo-Plastic (Johnson-Cook + Temperature) (`engine/source/materials/mat/mat070/`)

Johnson-Cook elastic-plastic with full thermo-mechanical coupling: plastic
dissipation heats the element, and temperature feeds back into σ_y(T).

## Key Files

| File | Role |
|------|------|
| `sigeps70.F` | Main stress update: JC hardening with T feedback |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
