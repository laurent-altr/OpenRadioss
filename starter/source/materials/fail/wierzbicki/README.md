# Wierzbicki Failure (`starter/source/materials/fail/wierzbicki/`)

Starter reader for /FAIL/WIERZBICKI: Hosford-Coulomb fracture locus in the triaxiality–Lode angle space.

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_wierzbicki.F` | Parse /FAIL/WIERZBICKI card: c1–c3 Hosford parameters |

## Description

The Wierzbicki (Hosford-Coulomb) criterion defines the fracture strain as `ε_f(η, θ̄)` where the locus is constructed from the Hosford yield surface intersected with the Mohr-Coulomb fracture plane. Damage accumulates as `D = ∫ dε_p / ε_f(η, θ̄)`.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/wierzbicki/README.md` — runtime failure evaluation
