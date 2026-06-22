# Materials Subsystem

This subsystem implements all constitutive models (material laws), failure criteria, and viscosity models used by the engine. It is one of the largest subsystems, containing over 110 material law directories.

## Directory Structure

```
materials/
├── mat/          — Individual material law implementations
├── mat_share/    — Shared routines called by multiple material laws
├── fail/         — Failure criteria (damage / rupture models)
└── visc/         — Viscosity / rate-dependent models
```

## Material Laws (`mat/`)

Each subdirectory `mat<NNN>/` implements `/MAT/LAW<NNN>`. The main computation routine is typically named `sigeps<NNN>*.F` (stress-strain integration) and called by the element loop.

| Law range | Typical material class |
|-----------|----------------------|
| LAW001–LAW006 | Elastic, elastic-plastic (bilinear, Johnson-Cook, piecewise) |
| LAW010–LAW028 | Advanced metals, composites, foam, rubber |
| LAW032–LAW059 | Polymers, crushable foam, honeycomb, fabric |
| LAW060–LAW097 | Multi-layer composites, soils, concrete, wood |
| LAW100–LAW134 | Advanced composites, damage, EOS-coupled, user materials |
| LAW158–LAW190 | Specialist models (biological tissue, adhesives, …) |

Shared helper routines for multiple laws live in `mat_share/` (e.g. `mmain.F90`, `meos8.F` for EOS integration, `fmqviscb.F` for viscous corrections).

## Failure Criteria (`fail/`)

Each subdirectory or file implements a `/FAIL/<name>` or `/FAIL/LAW<N>` criterion.

| Criterion | Description |
|-----------|-------------|
| `johnson_cook/` | Johnson-Cook ductile failure |
| `cockroft_latham/` | Cockcroft-Latham damage integral |
| `hashin/` | Hashin composite failure |
| `tsaihill/`, `tsaiwu/` | Tsai-Hill / Tsai-Wu composite criteria |
| `puck/` | Puck inter-fibre failure |
| `ladeveze/` | Ladevèze composite damage |
| `lemaitre/` | Lemaitre continuum damage mechanics |
| `fld/` | Forming Limit Diagram (sheet metal) |
| `fabric/` | Fabric failure |
| `changchang/` | Chang-Chang composite |
| `energy/` | Energy-based failure |
| `composite/` | General composite failure |
| `tabulated/` | Tabulated failure surface |
| `wilkins/` | Wilkins ductile damage |
| `wierzbicki/` | Wierzbicki-Xue ductile fracture |
| `hc_dsse/` | HC/DSSE criterion |
| `max_strain/`, `tensstrain/` | Maximum strain criteria |
| `spalling/` | Spall failure |
| `emc/` | EMC criterion |
| `biquad/`, `orthbiquad/`, `orthstrain/`, `orthenerg/` | Orthotropic failure criteria |

Shared setoff routines (`fail_setoff_c.F`, `fail_setoff_npg_c.F`) manage failure flag propagation across integration points.

## Viscosity Models (`visc/`)

| File | Model |
|------|-------|
| `viscmain.F` | Dispatcher — selects viscosity law by type |
| `visc_prony.F` | Prony series viscoelasticity |
| `visc_prony_lstrain.F` | Prony series at large strains |
| `visc_plas.F90` | Viscoplastic correction |
| `prony_modelc.F` | Compressed Prony model data |

## Call Pattern

The element loop (in `elements/`) calls the material integration via:

```
SIGEPS<NNN>   ←  main stress update for LAW<NNN>
    └── SIGEPS<NNN>C / SIGEPS<NNN>T  (compression / tension split, where applicable)
         └── M<NNN>CPLR / sub-routines in mat_share/
```

Failure is checked after stress update; on failure `OFF(i)=0` marks the element as eroded.

## Related Documentation

- `engine/source/elements/README.md` — how elements call material laws
- `common_source/eos/README.md` — Equation of State models (called from some material laws)
- `common_source/fail/README.md` — shared failure utilities
