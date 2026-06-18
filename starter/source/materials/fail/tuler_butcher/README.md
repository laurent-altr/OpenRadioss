# Tuler-Butcher Failure (`starter/source/materials/fail/tuler_butcher/`)

Starter reader for /FAIL/TBUTCHER: Tuler-Butcher spall failure criterion based on cumulative stress impulse.

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_tbutcher.F` | Parse /FAIL/TBUTCHER card: σ₀, λ, K_crit parameters |

## Description

The Tuler-Butcher criterion accumulates `K = ∫ ⟨σ − σ₀⟩^λ dt` and fails the element when `K ≥ K_crit`. Primarily used for dynamic fracture and spallation under pressure pulses.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/tuler_butcher/README.md` — runtime failure evaluation
