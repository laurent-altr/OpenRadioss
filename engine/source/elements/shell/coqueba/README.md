# Belytschko-Tsay Shell (`engine/source/elements/shell/coqueba/`)

Quadrilateral shell using the Belytschko-Tsay (BT) formulation: 1-point quadrature with viscous/stiffness hourglass control.

## Key Files

| File | Role |
|------|------|
| `cbacoor.F` | Co-rotational coordinate frame update |
| `cbadef.F` | Strain computation (BT kinematics) |
| `cbafint_reg.F` | Internal force with regularised hourglass |
| `cbaforc3.F` | Force assembly |
| `cbaener.F` | Energy computation |
| `cbafori.F` | BT forces for implicit solver |
| `cbacoork.F` / `cbacoorkpinch.F` | Coordinates with pinch-force variant |
| `cbadefpinch.F` | Strain for pinch-force case |

## Formulation

The Belytschko-Tsay (1984) formulation uses one-point quadrature and the co-rotational frame. Hourglass control is via viscous stabilisation (`Qhg × ρ × c × h` proportional), which is less accurate than QEPH for bending but faster per element. BT is the industry-standard shell for explicit crash (used in LS-DYNA as the default). In OpenRadioss QEPH is generally preferred; BT is retained for compatibility and benchmark studies.

## Related Documentation

- `engine/source/elements/shell/README.md` — parent shell directory
- `engine/source/elements/shell/coque/README.md` — QEPH (preferred formulation)
