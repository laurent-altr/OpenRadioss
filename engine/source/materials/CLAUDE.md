# engine/source/materials/

## Purpose
All material constitutive laws: stress computation, equation of state (EOS),
failure models, viscosity, and shared utilities. The material dispatcher
`MMAIN` / `MMAIN8` is called from element kernels (e.g., `SFORC3`, `CFORC3`)
for each integration point each cycle.

## Directory structure

```
materials/
  mat/          119 subdirectories mat001..mat190 — one per material law
  mat_share/    Shared dispatcher + utility routines called by all laws
  fail/         Failure model library (independent of material law)
  visc/         Viscosity models (rate-dependent response)
  tools/        Material utility routines (matrix math, table interpolation)
```

---

## `mat_share/` — Dispatchers and shared utilities

| File | Role |
|------|------|
| `mmain.F90` | **Main material dispatcher** for solids: reads `ilaw` from `ELBUF_TAB(NG)%BUFLY(IL)%ILAW`; calls `MXLAW` for law X |
| `mmain8.F` | Shell material dispatcher (8-noded thick shell variant) |
| `mulawc.F90` | Shell material dispatcher for coque4n shells (calls `CMAIN3`) |
| `mulaw.F90`, `mulaw8.F90` | Material law dispatchers for other element types |
| `mulawglc.F` | Dispatcher for global (composite) material calculation |
| `cmain3.F` | Shell-specific material main routine |
| `meint.F` | Internal energy computation |
| `meos8.F` | EOS (Equation of State) dispatcher for 8 integration points |
| `mdtsph.F` | SPH material time-step contribution |
| `mstrain_rate.F` | Strain rate computation |
| `mtheta.F` | Theta (Jaumann) rate rotation |
| `mrotens.F` | Tensor rotation |
| `mreploc.F` | Local frame replacement |
| `mnsvis.F`, `mqvisc8.F`, `mqviscb.F`, `fmqviscb.F`, `mqvisc26.F` | Viscous (artificial) stress contributions |
| `jacobview_v.F` | Jacobian view for verification |
| `prony_modelc.F` | Prony series (viscoelastic) computation shared across laws |

---

## `mat/` — Material law implementations (119 laws)

### Uniform pattern
Each `matXXX/` directory contains:
- `mXXXlaw.F` (or `mNNlaw.F` without leading zeros) — main constitutive update
- Optional: `sigepsXXX.F` — yield stress vs. plastic strain
- Optional: `mXXXlaw_imp.F` — implicit solver variant
- Optional: `mXXXlaw_pon.F` — Parith/ON variant

The dispatcher `MMAIN` reads `ilaw = ELBUF_TAB(NG)%BUFLY(IL)%ILAW` and calls
`M{ilaw}LAW(…)`. Each law receives the stress tensor `lbuf%sig`, strain increments,
and material parameters from `PM` (the material parameter array from the Starter).

### Key material laws

| Law | Physical model |
|-----|---------------|
| 1 | Elastic (isotropic) — linear Hooke's law |
| 2 | Elastic-plastic (Johnson-Cook isotropic hardening with rate effects) |
| 3 | Elastic-plastic (isotropic, tabulated yield curve) |
| 4 | Elastic-plastic (isotropic, Cowper-Symonds) |
| 5 | JWL explosive (Jones-Wilkins-Lee EOS for high explosives) |
| 6 | Elastic-plastic (isotropic, simplified rate-dependent) |
| 10 | Drucker-Prager (soil/concrete, pressure-dependent yield) |
| 11 | Bounded elastic (user-defined stress bounding) |
| 12 | Elastic-plastic (composite tabulated) |
| 14 | Elastic-plastic (rubber, neo-Hookean) |
| 15 | Elastic-plastic (composite solid) |
| 16 | Johnson-Cook-Gray (adiabatic, rate + temperature) |
| 17 | Elastic-plastic with damage (Lemaitre CDM) |
| 18 | Elastic-plastic (tabulated strain rate) |
| 19 | Tabulated elastic-plastic with optional failure |
| 21 | Elastic-plastic with kinematic hardening (Bauschinger) |
| 22 | Composite orthotropic (Chang-Chang failure) |
| 24 | Concrete (Winfrith or Krauthammer model) |
| 25 | Elastic-plastic with tabulated hardening + damage |
| 26 | Elastic-plastic (bilinear, rate-dependent) |
| 27 | Elastic-plastic (semi-analytic) |
| 28 | Elastic-plastic + viscous (Bingham fluid) |
| 32 | Laminated shell composite (fiber + matrix) |
| 33 | Elastic-plastic (Voce hardening) |
| 34 | Elastic (orthotropic, Hill anisotropy) |
| 35 | Elastic-plastic orthotropic (Hill + Voce) |
| 36 | Elastic-plastic (tabulated, multiple curves) |
| 38 | Foam (crushable open/closed-cell foam) |
| 42 | Drucker-Prager (general, pressure dependent) |
| 43 | Elastic-plastic (Hill 1948 anisotropy) |
| 44 | Elastic-plastic (Barlat 1989, sheet anisotropy) |
| 45 | Elastic-plastic (Krupkowski power law) |
| 46 | Elastic-plastic (Yoshida-Uemori cyclic plasticity) |
| 48 | Rubber (Mooney-Rivlin) |
| 49 | Elastic-plastic (tabulated — very widely used) |
| 50 | Elastic (linear, isotropic, small strain) |
| 51 | Elastic-plastic (rate-dependent, tabulated) |
| 52 | Elastic-plastic (Prager kinematic hardening) |
| 53 | Elastic-plastic (Krupkowski, rate-dependent) |
| 54 | Elastic-plastic (orthotropic Barlat Yld2000) |
| 57 | Closed-cell foam (viscous, strain-rate dependent) |
| 58 | Fabric (orthotropic membrane, non-linear) |
| 59 | Foam (viscoelastic, Ogden) |
| 60 | Elastic-plastic (Mohr-Coulomb) |
| 62 | Viscoelastic (hyperelastic, Ogden-Prony) |
| 63 | Elastic-plastic (modified Johnson-Cook) |
| 64 | Elastic-plastic (Hockett-Sherby) |
| 65 | Elastic-plastic (Zerilli-Armstrong) |
| 66 | Elastic-plastic (3-component foam) |
| 69 | Elastic-plastic (Lemaitre ductile damage, rate-dependent) |
| 70 | Hyperelastic (Ogden) |
| 71 | Honeycomb (orthotropic crush) |
| 72 | Fabric (airbag, non-linear orthotropic) |
| 74 | Elastic-plastic (tabulated + failure by Cockroft-Latham) |
| 75 | Elastic-plastic with XFEM crack |
| 76 | Elastic-plastic (Hill + rate) |
| 77 | Elastic-plastic (Barlat Yld89 + rate) |
| 78 | Elastic-plastic (Vegter) |
| 79 | Elastic-plastic (Barlat Yld2000 + rate) |
| 80 | Rubber (Arruda-Boyce 8-chain) |
| 82 | Elastic-plastic (tabulated + damage) |
| 83 | Elastic-plastic (Chang-Chang composite failure) |
| 84 | Composite (Puck-Schürmann criterion) |
| 85 | Composite (Ladevèze delamination-capable) |
| 86 | Elastic-plastic (Barlat Yld2004-18p) |
| 88 | Hydrodynamic (user EOS + Mie-Grüneisen) |
| 90 | Elastic-plastic (Drucker-Prager + cap) |
| 91 | Porous (Gurson-Tvergaard-Needleman) |
| 92 | Elastic-plastic (tabulated isotropic + kinematic) |
| 93 | Elastic-plastic (orthotropic Hill + tabulated) |
| 94 | Elastic-plastic (tabulated + Barlat Yld89) |
| 95 | Elastic-plastic (CBM composite) |
| 96 | Elastic-plastic (tabulated + Yoshida cyclic) |
| 97 | Elastic-plastic (CBM + rate effects) |
| 100 | XFEM composite |
| 101 | Composite (Hashin 3D) |
| 102 | Composite (MCT MultiContinuum Theory) |
| 103 | Foam (crushable, smooth tabulated) |
| 104 | Concrete (Drucker-Prager CDM) |
| 105 | Elastic-plastic (tabulated, Hershey-Hosford) |
| 106 | Rubber (Sussman-Bathe incompressible) |
| 107 | Elastic-plastic (Barlat Yld2004 plane stress) |
| 109 | Viscoplastic (Bodner-Partom) |
| 110 | Elastic-plastic (non-associated Hill) |
| 111 | Foam (tabulated volumetric) |
| 115 | Composite (LARC05 criterion) |
| 116 | Elastic-plastic (CBM + cyclic) |
| 117 | Rubber (general hyperelastic) |
| 119 | Elastic-plastic (tabulated + Cockroft-Latham) |
| 120 | Composite (Maimi orthotropic CDM) |
| 121 | Elastic-plastic (rate + temperature table) |
| 122 | Elastic-plastic (Cazacu-Plunkett-Barlat asymmetric) |
| 123 | Elastic-plastic (Teodosiu-Hu microstructure evolution) |
| 124 | Concrete (CDPM2 — Concrete Damage Plasticity) |
| 125 | Elastic-plastic (Barlat BBC2008) |
| 126 | Elastic-plastic (CBM anisotropic) |
| 127 | Elastic-plastic (tabulated orthotropic) |
| 128 | Composite (Cuntze criterion) |
| 129 | Elastic-plastic (Hu 2003 asymmetric) |
| 130 | Elastic-plastic (modified Mohr-Coulomb) |
| 131 | Elastic-plastic (Vegter-Cui) |
| 132 | Elastic-plastic (Hershey-Hosford + rate) |
| 133 | Composite (Christensen criterion) |
| 134 | Rubber (penalty incompressible) |
| 158 | Porous metal (Gurson + Lemaitre) |
| 163 | Elastic-plastic (Drucker-Prager cap) |
| 169 | Elastic-plastic (CBM2) |
| 187 | Elastic-plastic (Chaboche + fatigue) |
| 190 | Elastic-plastic (tabulated, extended) |

---

## `fail/` — Failure models

Independent of material law; applied after stress update. Contains subdirectories
for each failure criterion:

| Sub-dir | Criterion |
|---------|-----------|
| `alter/` | ALTER (effective strain with stress triaxiality) |
| `biquad/` | Bi-quadratic FLD |
| `changchang/` | Chang-Chang fiber/matrix failure |
| `cockroft_latham/` | Cockroft-Latham ductile fracture |
| `composite/` | Composite failure (Hashin, Puck, etc.) |
| `connect/` | Connect failure (rivets/welds) |
| `emc/` | EMC (Energy-based Micro Crack) |
| `energy/` | Energy-based failure |
| `fabric/` | Fabric failure |
| `failwave/` | Failure wave propagation |
| `fld/` | Forming Limit Diagram |
| `gene1/` | GENE1 generalized failure |
| `hashin/` | Hashin fiber/matrix criteria |
| `hc_dsse/` | HC-DSSE (semi-empirical) |
| `hoffman/` | Hoffman orthotropic failure |
| `inievo/` | Initial evolution failure |
| `johnson_cook/` | Johnson-Cook fracture |
| `prony_modelc.F` | Prony model for viscoelastic failure |
| `visc_plas.F90` | Viscoplastic failure |
| `visc_prony*.F` | Viscoelastic Prony failure |
| `fail_setoff_c.F` | Element deletion after all GPs fail |
| `fail_setoff_npg_c.F` | Fail based on number of failed GPs |

---

## `visc/` — Viscosity models

Applied as an additional deviatoric stress to the material update:
- `viscmain.F` — viscosity dispatcher
- `visc_plas.F90` — viscoplastic law
- `visc_prony.F` — Prony viscoelastic law
- `visc_prony_lstrain.F` — large-strain Prony

---

## `tools/` — Utility routines

| File | Role |
|------|------|
| `kmatinv.F` | Small matrix inversion (3×3, 6×6) |
| `prodAAT.F`, `prodATA.F`, `prodmat.F` | Matrix products |
| `roto_tens2d.F`, `roto_tens2d_aniso.F` | 2D tensor rotation |
| `table_mat_vinterp.F` | Multi-variable table interpolation for material properties |
| `read_mat_table.F` | Reads material table data from restart |

---

## Dispatch flow

```
Element kernel (e.g., SFORC3)
  → MMAIN8(ELBUF_TAB, NG, IL, IR, IS, IT, …)
       ilaw = ELBUF_TAB(NG)%BUFLY(IL)%ILAW
       SELECT on ilaw:
         1  → M1LAW(…)   ! elastic
         2  → M2LAW(…)   ! elasto-plastic Johnson-Cook
         49 → M49LAW(…)  ! tabulated elasto-plastic
         …
       → FAIL dispatcher (if NFAIL > 0)
       → VISC dispatcher (if IVISC > 0)
       → EOS dispatcher (if IEOS > 0, via MEOS8)
```

---

## Dependencies
- Called by: element kernels in `engine/source/elements/` (solid, shell, beam, spring)
- Uses: `mat_elem_mod.F90` (re-exports `elbufdef`, `matparam_def`, `group_param`, `prop_param`)
- Material parameters read by Starter into `PM` array (passed as `mat_elem%mat_param`)
