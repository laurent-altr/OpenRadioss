# Extended Boundary Conditions (`engine/source/boundary_conditions/ebcs/`)

Computes extended boundary condition (EBCS) forces applied to element face surfaces each time step.

## Key Files

| File | Role |
|------|------|
| `ebcs_main.F` | Main EBCS dispatcher: call appropriate routine for each EBCS type |
| `ebcs4_vel.F` | TYPE4 — non-reflecting (absorbing) BC: damp outgoing waves |
| `ebcs5_normv.F` | TYPE5 — normal velocity prescribed on a surface |
| `ebcs6_inip.F` | TYPE6 — initial pressure on a surface |
| `ebcs7_iniv.F` | TYPE7 — initial velocity on a surface |
| `ebcs8_inlet.F90` | TYPE8 — flow inlet/outlet: prescribe velocity/pressure at surface |
| `ebcs10_nrf.F` | TYPE10 — NRF absorbing BC (Non-Reflecting Far-field) |
| `ebcs11_propellant.F90` | TYPE11 — propellant burn pressure BC (solid rocket motor) |
| `ebcs12_cyclic.F90` | TYPE12 — cyclic symmetry on a surface (3D periodic) |
| `ebcs123_pres.F` | TYPE1/2/3 — prescribed pressure on a surface |
| `ebcclap.F` | CLAP — clapotis (standing wave) pressure BC for offshore |
| `ebcs_extrapol.F` | Extrapolate field variables to surface nodes for EBCS evaluation |
| `ebcs_vol2seg.F` | Convert volume quantities (pressure, velocity) to surface segment values |
| `ebcvit4.F` | Velocity evaluation for TYPE4 absorbing BC |
| `ebcvit5.F` | Velocity evaluation for TYPE5 |
| `ebcvit7.F` | Velocity evaluation for TYPE7 |
| `ebcs0_gradp0.F` | Zero-gradient pressure BC (free surface) |

## TYPE10 NRF Absorbing BC

The Non-Reflecting Far-field boundary absorbs outgoing elastic/acoustic waves without reflection:
```
F_BC = -ρ c A v_normal
```
where `ρ c` is the acoustic impedance and `v_normal` is the outward normal velocity. This prevents artificial reflections from model boundaries in wave propagation problems.

## Related Documentation

- `engine/source/boundary_conditions/README.md` — parent boundary conditions directory
- `starter/source/boundary_conditions/README.md` — EBCS reading and setup
