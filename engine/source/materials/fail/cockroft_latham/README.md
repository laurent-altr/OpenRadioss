# Cockcroft-Latham Failure Criterion (`engine/source/materials/fail/cockroft_latham/`)

Ductile fracture criterion based on the integral of maximum tensile stress over equivalent plastic strain (Cockcroft-Latham 1968).

## Key Files

| File | Role |
|------|------|
| `fail_cockroft_c.F` | Cockcroft-Latham for solid elements |
| `fail_cockroft_s.F` | Cockcroft-Latham for shell elements |
| `fail_cockroft_ib.F` | Cockcroft-Latham for implicit beam elements |

## Criterion

Failure when the accumulated damage integral exceeds a critical value:

```
C = ∫₀^ε_f ⟨σ_max⟩ dε_p ≥ C_crit
```

where `⟨σ_max⟩ = max(σ_1, 0)` is the positive maximum principal stress (Macaulay bracket). This criterion captures the fact that ductile fracture in metal forming is promoted by tensile stress and suppressed by hydrostatic compression, making it more physically motivated than simple strain thresholds for bulk metal forming.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/rtcl/README.md` — Rice-Tracey-Cockcroft-Latham variant
- `engine/source/materials/fail/johnson_cook/README.md` — triaxiality-based ductile fracture
