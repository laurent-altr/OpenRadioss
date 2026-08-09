# Forming Limit Diagram (FLD) Failure Criterion (`engine/source/materials/fail/fld/`)

Implements the Forming Limit Diagram criterion for sheet metal stamping: failure when the principal strain path crosses the FLD curve.

## Key Files

| File | Role |
|------|------|
| `fail_fld_c.F` | FLD criterion for solid elements |
| `fail_fld_tsh.F` | FLD criterion for thick-shell elements |
| `fail_fld_xfem.F` | FLD driving XFEM crack in forming simulation |

## Criterion

The FLD is a curve `ε_2 = f(ε_1)` in principal strain space `(ε_1 ≥ ε_2)` separating safe from failed states. The curve is user-provided as a `/FUNCT` table or calculated analytically (Keeler-Goodwin). At each integration point:

1. Compute principal in-plane strains `ε_1, ε_2` at the current step
2. Compare against the interpolated FLD limit at the same `ε_1`
3. If `ε_2 > FLD(ε_1)`: element fails (necking instability detected)

`fail_fld_xfem.F` uses the FLD indicator to trigger XFEM level-set advancement, modelling localised neck propagation through the sheet.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/elements/xfem/README.md` — XFEM crack propagation
