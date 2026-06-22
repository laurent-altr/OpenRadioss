# LAW131 — Modular Elastic-Plastic Framework (`engine/source/materials/mat/mat131/`)

LAW131 is a fully modular elastic-plastic material: each physical mechanism
(elasticity, yield criterion, hardening, rate dependence, thermal softening)
is a separate interchangeable Fortran 90 module. Any combination can be
selected at runtime.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `elasticity/` | Isotropic, orthotropic, anisotropic, T-dependent, bimodular, viscous |
| `yield_criterion/` | Von Mises, Hill, Hershey, Barlat1989, Barlat2000, BBC2005 |
| `work_hardening/` | Voce, linear-Voce, power-law, tabulated |
| `kinematic_hardening/` | Prager-Ziegler, Chaboche multi-surface |
| `srate_dependency/` | Johnson-Cook, Cowper-Symonds, nonlinear, tabulated |
| `therm_softening/` | Johnson-Cook, Zhao, tabulated |
| `self_heating/` | Taylor-Quinney, tabulated β |
| `return_mapping/` | CPPM (Newton), cutting-plane, NICE (vectorised) |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
