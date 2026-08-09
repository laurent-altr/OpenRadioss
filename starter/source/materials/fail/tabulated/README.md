# Tabulated Failure (`starter/source/materials/fail/tabulated/`)

Starter readers for /FAIL/TAB1 and /FAIL/TAB2: tabulated failure strain as a function of stress state (triaxiality, Lode angle, strain rate, temperature).

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_tab1.F` | Parse /FAIL/TAB1: 1D tabulated failure strain vs. triaxiality |
| `hm_read_fail_tab2.F` | Parse /FAIL/TAB2: 2D tabulated failure strain vs. triaxiality and Lode angle |
| `hm_read_fail_tab_old.F` | Read legacy tabulated failure format |

## Description

Tabulated failure allows arbitrary failure loci without a parametric formula. `/FAIL/TAB2` provides the most general form: `ε_f = f(η, θ̄, ε̇, T)` where `η = σ_m/σ_eq` (triaxiality) and `θ̄` is the normalised Lode angle. The table is interpolated bilinearly at runtime.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/tabulated/README.md` — runtime failure evaluation
