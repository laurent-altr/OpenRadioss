# eos/

## Purpose
Equation of State (EOS) model implementations. Each EOS type has its own Fortran module; `eosmain.F` is the dispatcher that selects the correct model at runtime. Shared by all material laws that require a pressure-density-energy relation.

## Files

### Main dispatcher
| File | Description |
|------|-------------|
| `eosmain.F` | Module `EOSMAIN_MOD` — subroutine `eosmain` dispatches to the appropriate EOS based on material type; handles staggered scheme (IFLAG=0,1) and collocated scheme (IFLAG=2); manages pressure/energy derivatives |

### Physical EOS models
| File | Module | Description |
|------|--------|-------------|
| `idealgas.F` | `IDEALGAS_MOD` | Ideal gas (gamma law) |
| `idealgas_vt.F` | `IDEALGAS_VT_MOD` | Ideal gas with temperature-dependent Cp(T) |
| `stiffgas.F` | `STIFFGAS_MOD` | Stiffened gas (Mie-Grüneisen variant for liquids) |
| `noble_abel.F` | `NOBLE_ABEL_MOD` | Noble-Abel gas (real-gas correction) |
| `nasg.F` | `NASG_MOD` | Noble-Abel Stiffened Gas with 2nd-order time integration |
| `gruneisen.F` | `GRUNEISEN_MOD` | Grüneisen EOS for solid materials under compression |
| `murnaghan.F` | `MURNAGHAN_MOD` | Murnaghan EOS for solid compression |
| `tillotson.F` | `TILLOTSON_MOD` | Tillotson multi-region EOS (compression, cold/hot expansion, transition) |
| `jwl.F` | `JWL_MOD` | Jones-Wilkins-Lee EOS for explosive detonation products |
| `puff.F` | `PUFF_MOD` | PUFF EOS |
| `lszk.F` | `LSZK_MOD` | Landau-Stanyukovich-Zeldovich-Kompaneet EOS |
| `osborne.F` | `OSBORNE_MOD` | Osborne EOS with density-dependent behaviour |
| `eoslinear.F` | `EOSLINEAR_MOD` | Linear EOS (parameters C0 and BULK) |
| `eospolyno.F` | `EOSPOLYNO_MOD` | Polynomial EOS |
| `eosexponential.F90` | `eosexponential_mod` | Exponential EOS: P = P0·exp(α·t) − PSH |
| `powder_burn.F` | `POWDER_BURN_MOD` | Deflagration EOS (experimental) for burning powder (LAW105) |

### Tabulated / data-driven EOS models
| File | Module | Description |
|------|--------|-------------|
| `tabulated.F` | `TABULATED_MOD` | Tabulated EOS: P(µ,E) = A(µ) + B(µ)·E using user-defined function tables |
| `sesame.F` | `SESAME_MOD` | SESAME table interpolation EOS (calls `MINTP1_RT`, `MINTP_RE`, `MINTP_RT`) |

### Compaction EOS models
| File | Module | Description |
|------|--------|-------------|
| `compaction.F90` | `compaction_mod` | Compaction EOS with elastic limits (MU_MIN, MU_MAX) and unload path |
| `compaction2.F90` | `compaction2_mod` | Alternative compaction EOS formulation |
| `compaction_tab.F90` | `compaction_tab_mod` | Tabulated compaction EOS: density-dependent compaction/unloading curves with gamma factor |

### Interpolation helpers (used by SESAME and tabulated EOS)
| File | Description |
|------|-------------|
| `minter1d_rat.F` | Subroutine `MINTER1D_RAT` — 1D rational function interpolation |
| `mintp_re.F` | Subroutine `MINTP_RE` — 2D table interpolation with respect to energy E |
| `mintp_rt.F` | Subroutine `MINTP_RT` — 2D table interpolation returning Z and dZ/dX |
| `mintp1_rt.F` | Subroutine `MINTP1_RT` — 2D table interpolation returning Z, dZ/dX, dZ/dY |

## Key Types Exported
- Each EOS module exports a single subroutine with the same name as the module root (e.g., `GRUNEISEN`, `JWL`).
- EOS parameter structures are defined in `modules/mat_elem/eos_param_mod.F90`.

## Dependencies
- Uses: `modules/mat_elem/eos_param_mod.F90` (EOS parameter types), `modules/precision_mod.F90`
- Used by: material law implementations in engine `source/mater/` and `source/elements/`; dispatched via `eosmain`
