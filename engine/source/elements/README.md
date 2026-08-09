# Elements Subsystem

This subsystem contains all finite element formulations used by the engine. Each element type computes internal forces, stiffness contributions, and (for shell/solid elements) calls the material law integration.

## Directory Structure

```
elements/
├── beam/          — Beam elements (TYPE3, TYPE18)
├── joint/         — Joint / spring-like connectors
├── rivet/         — Rivet / spot-weld elements
├── sh3n/          — 3-node triangular shell elements
├── shell/         — 4-node quadrilateral shell elements (main shell formulation)
├── solid/         — 3D solid elements (all formulations)
├── solid_2d/      — 2D solid elements (plane stress / plane strain / axisymmetric)
├── sph/           — Smoothed Particle Hydrodynamics
├── spring/        — 1D spring / dashpot elements
├── thickshell/    — Thick shell elements (degenerated solid)
├── truss/         — Truss / bar elements (TYPE3 no bending)
├── ige3d/         — IGE3D (improved solid element)
├── xelem/         — Extended element framework
└── xfem/          — eXtended Finite Element Method (crack propagation)
```

Individual dispatch files at the top level (`forint.F`, `forintc.F`, `forintp.F`, `forints.F`, `desacti.F`, `eloff.F`, `findgroup.F`) route element group computations to the correct type-specific routines.

## Shell Elements (`shell/`)

The shell subsystem implements the Belytschko-Tsay and QEPH formulations.

| Subdirectory | Content |
|-------------|---------|
| `coque/` | Core shell element computation (QEPH, Belytschko-Tsay) |
| `coqueba/` | Belytschko-Tsay variant for specific configurations |
| `coquez/` | Shell element with nodal thickness |
| `coqini.F` | Shell group initialisation |
| `err_thk.F` | Thickness error checking |

The inner loop for a shell element:
1. Interpolate kinematics (velocities → strain rates) at integration points
2. Call material law `SIGEPS<NNN>` to update stresses
3. Integrate stresses to nodal forces

## Solid Elements (`solid/`)

| Subdirectory | Formulation |
|-------------|-------------|
| `solide/` | General 8-node brick (HEPH, QEPH, standard) |
| `solide4/` | 4-node tetrahedron |
| `solide4_sfem/` | Smoothed FEM tetrahedron |
| `solide6z/` | 6-node pentahedron |
| `solide8/` | 8-node hex (reduced integration) |
| `solide8e/` | 8-node hex enhanced assumed strain |
| `solide8s/` | 8-node hex selective reduced integration |
| `solide8z/` | 8-node hex with Zienkiewicz-Zhu stabilisation |
| `solidez/` | General solid with hourglass control |
| `solide10/` | 10-node tetrahedron (quadratic) |
| `solide20/` | 20-node hex (quadratic) |
| `sconnect/` | Solid connector elements |
| `srotorth.F` | Orthotropic rotation update |

## Beam Elements (`beam/`)

| File | Description |
|------|-------------|
| `main_beam3.F` | TYPE3 simple beam / truss |
| `main_beam18.F` | TYPE18 Timoshenko beam (shear-deformable) |
| `fail_beam3.F`, `fail_beam18.F` | Beam failure checks |
| `mulaw_ib.F` | Multi-law beam integration |

## Spring / Dashpot (`spring/`)

1D spring, dashpot, and general spring-mass elements. Stiffness and force are computed from tabulated or analytical force-displacement / force-velocity laws.

## SPH (`sph/`)

Smoothed Particle Hydrodynamics: kernel-based interpolation replaces the element mesh. Used for very large deformations (fluid-like behaviour, fragmentation).

## XFEM (`xfem/`)

eXtended FEM for crack propagation in solid elements. Enriches the displacement field with discontinuous basis functions; crack growth is tracked at the element level.

## Hourglass Control

Reduced-integration elements require hourglass (zero-energy mode) stabilisation. Several methods are available (`HEPH` — hourglass energy penalty, `QEPH` — physical stabilisation). The control parameter is set in the property type.

## Call Pattern from the Engine Time Loop

```
resol.F
  └── FORINT  (forint.F)            — dispatch by element type
        ├── COQUE_MAIN              — shell group loop (OpenMP)
        │     └── SIGEPS<NNN>       — material law
        ├── SOLIDE_MAIN             — solid group loop (OpenMP)
        │     └── SIGEPS<NNN>
        ├── MAIN_BEAM18             — beam group loop
        └── ...
```

## Related Documentation

- `engine/source/materials/README.md` — constitutive laws called by elements
- `engine/source/assembly/README.md` — how element forces are assembled to nodes
- `tools/mockup/element_sandbox/Readme.md` — standalone mockup for studying element computation
