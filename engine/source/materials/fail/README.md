# Failure Criteria (`engine/source/materials/fail/`)

Implements all element failure and deletion criteria: each subdirectory contains one criterion evaluated each step at every active integration point.

## Subdirectories

| Directory | Criterion | Description |
|-----------|-----------|-------------|
| `johnson_cook/` | Johnson-Cook | Ductile fracture: triaxiality + strain rate + temperature |
| `hashin/` | Hashin | Composite: fiber/matrix mode-separated quadratic criteria |
| `puck/` | Puck | Composite: physically based inter-fiber fracture with fracture plane |
| `lemaitre/` | Lemaitre CDM | Coupled damage: scalar damage reduces stiffness progressively |
| `ladeveze/` | Ladeveze CDM | Intralaminar CDM with independent fiber/matrix/shear damage |
| `changchang/` | Chang-Chang | Composite: Hashin-based with stiffness degradation |
| `tsaiwu/` | Tsai-Wu | Composite: quadratic tensor polynomial, single failure surface |
| `tsaihill/` | Tsai-Hill | Composite: Hill quadratic, symmetric tension/compression strengths |
| `hoffman/` | Hoffman | Composite: Hoffman generalisation with asymmetric strengths |
| `biquad/` | Bi-quadratic | Isotropic: elliptic failure surface in principal strain space |
| `orthbiquad/` | Orthotropic bi-quad | Orthotropic bi-quadratic in material axes |
| `max_strain/` | Maximum strain | Element deletion at strain component thresholds |
| `orthstrain/` | Orthotropic max strain | Max strain in material (fiber/transverse) axes |
| `energy/` | Energy | Failure when accumulated specific internal energy exceeds limit |
| `orthenerg/` | Orthotropic energy | Energy accumulation in material axes (fiber/matrix independent) |
| `fld/` | Forming Limit Diagram | Sheet metal: principal strain path crosses FLD curve |
| `tabulated/` | Tabulated | User table: `ε_f(triaxiality)` or 2D `ε_f(triaxiality, Lode)` |
| `composite/` | Composite (general) | Multi-mode composite failure with ply-by-ply degradation |
| `fabric/` | Fabric | Woven fabric: independent warp/weft/shear strain limits |
| `gene1/` | GENE1 | Configurable: weighted combination of stress/strain/energy indices |
| `cockroft_latham/` | Cockcroft-Latham | Integral of max tensile stress over plastic strain |
| `rtcl/` | RTCL | Combined Rice-Tracey (voids) + Cockcroft-Latham (shear) |
| `wierzbicki/` | Hosford-Coulomb | Fracture locus in (triaxiality, Lode angle) space |
| `wilkins/` | Wilkins | Dynamic: hydrostatic tension + deviatoric asymmetry |
| `tuler_butcher/` | Tuler-Butcher | Cumulative stress-impulse spall criterion |
| `spalling/` | Spalling | Instant failure when tensile pressure exceeds spall strength |
| `tensstrain/` | Tensile strain | Max principal tensile strain (Macaulay bracket) |
| `connect/` | Connector | Spring/joint failure by force or displacement limit |
| `snconnect/` | Shell-node connector | Spot-weld peel + shear interaction criterion |
| `emc/` | EMC | Electromagnetic-coupled failure |
| `failwave/` | Failure wave | Geometric failure front advancing at prescribed speed |
| `alter/` | Alter | Specialist: Brokmann polymer, Wind glass fracture |
| `hc_dsse/` | HC-DSSE | Hosford-Coulomb + shear for AHSS fracture |
| `inievo/` | Initial evolution | Two-phase: initiation threshold + progressive softening |
| `mullins_or/` | Mullins-OR | Rubber: Ogden-Roxburgh Mullins effect failure |
| `visual/` | Visual | Damage indicator output only (no element deletion) |
| `nxt/` | NXT | Extended tabulated locus with regularisation |
| `sahraei/` | Sahraei | Battery cell crush: elliptic principal strain locus |
| `syazwan/` | Syazwan | Hyperelastic: principal stretch failure |

## File Naming Convention

Each criterion has per-element-type variants:
- `_c.F` / `_c.F90` — solid (continuum) elements
- `_s.F` / `_s.F90` — shell elements
- `_b.F` / `_b.F90` — beam elements
- `_ib.F` / `_ib.F90` — implicit beam elements
- `_xfem.F` — drives XFEM level-set crack propagation

## Related Documentation

- `engine/source/materials/README.md` — parent materials directory
- `engine/source/elements/xfem/README.md` — XFEM crack propagation driven by fail criteria
