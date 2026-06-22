# Tabulated Failure Criterion (`engine/source/materials/fail/tabulated/`)

General-purpose failure criterion using user-supplied tables: failure strain (or stress) as a function of stress triaxiality, strain rate, and temperature.

## Key Files

| File | Role |
|------|------|
| `fail_tab_c.F` | Tabulated failure for solid elements |
| `fail_tab_s.F` | Tabulated failure for shell elements |
| `fail_tab_xfem.F` | Tabulated failure driving XFEM crack |
| `fail_tab2_c.F` | Extended tabulated (TYPE2): 2D table `ε_f(triaxiality, Lode angle)` for solid |
| `fail_tab2_b.F90` | TYPE2 tabulated failure for beam elements |
| `fail_tab2_ib.F90` | TYPE2 tabulated failure for implicit beam elements |
| `fail_tab2_s.F` | TYPE2 tabulated failure for shell elements |
| `fail_tab_old_c.F` | Legacy tabulated failure for solids |
| `fail_tab_old_s.F` | Legacy tabulated failure for shells |
| `fail_tab_old_xfem.F` | Legacy tabulated failure driving XFEM |

## Description

The TYPE1 variant uses a 1D table `ε_f = f(σ*)` where `σ* = p/σ_eq` is triaxiality. The TYPE2 variant uses a 2D surface `ε_f(σ*, L)` where `L` is the Lode angle parameter, enabling calibration of fracture loci from plane-strain, axisymmetric notch, and shear tests simultaneously. The `_old` variants are legacy formats retained for backwards compatibility.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/johnson_cook/README.md` — analytic triaxiality model
- `engine/source/materials/fail/wierzbicki/README.md` — Hosford-Coulomb locus
