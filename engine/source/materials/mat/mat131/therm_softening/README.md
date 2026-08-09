# LAW131 — Thermal Softening Modules (`engine/source/materials/mat/mat131/therm_softening/`)

Modular thermal-softening functions reducing yield stress with temperature.

## Key Files

| File | Role |
|------|------|
| `therm_softening_johnsoncook.F90` | Johnson-Cook thermal factor: (1 − T*ᵐ) |
| `therm_softening_zhao.F90` | Zhao thermal softening: exponential decay |
| `therm_softening_tabulated.F90` | Tabulated σ_y(T) multiplier |

## Related Documentation

- `engine/source/materials/mat/mat131/README.md` — parent directory
