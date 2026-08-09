# DYNA2RAD Converter Implementation (`reader/source/dyna2rad/dyna2rad/`)

Per-keyword conversion modules implementing the LS-DYNA → Radioss translation. Each `convertXXX.h/.cxx` pair handles one category of LS-DYNA keywords.

## Key Files

| File | Role |
|------|------|
| `convertbcs.h/.cxx` | BCS (boundary conditions): `*BOUNDARY_*` → `/BCS`, `/IMPVEL` |
| `convertboxes.h/.cxx` | Box definitions: `*DEFINE_BOX` → `/BOX` |
| `convertcards.h/.cxx` | Control cards: `*CONTROL_*`, `*DATABASE_*` → `/ANALY`, `/DT`, `/OUTP` |
| `convertcontacts.h/.cxx` | Contact: `*CONTACT_*` → `/INTER/TYPE7`, `/INTER/TYPE24` etc. |
| `convertcontrolvols.h/.cxx` | Airbag control volumes: `*AIRBAG_*` → `/MONVOL` |
| `convertconstrainedinterpolations.h/.cxx` | RBE3: `*CONSTRAINED_INTERPOLATION` → `/RBE3` |
| `convertconstrainedjoints.h/.cxx` | Joints: `*CONSTRAINED_JOINT_*` → `/CYL_JOINT`, `/GJOINT` |
| `convertconstrainednode.h/.cxx` | Tied nodes: `*CONSTRAINED_NODE_SET` → `/RBE2` |
| `convertconstrainedspotwelds.h/.cxx` | Spot welds: `*CONSTRAINED_SPOTWELD` → `/RWALL`/connector |
| `convertcrosssections.h/.cxx` | Section output: `*DATABASE_CROSS_SECTION` → `/SECT` |
| `convertcurves.h/.cxx` | Curves: `*DEFINE_CURVE` → `/FUNCT` |
| `convertdampings.h/.cxx` | Damping: `*DAMPING_*` → `/DAMP` |
| `convertdefineelementdeath.h/.cxx` | Element deletion: `*DEFINE_ELEMENT_DEATH` → failure |
| `convertdefinehexspotweldassembly.h/.cxx` | Hex spotweld: `*DEFINE_HEX_SPOTWELD_ASSEMBLY` |
| `convertdefinetransform.h/.cxx` | Transformations: `*DEFINE_TRANSFORMATION` → `/TRANSFORM` |
| `convertelementmasses.h/.cxx` | Discrete masses: `*ELEMENT_MASS` → `/ADMAS` |
| `convertelements.h/.cxx` | Elements: `*ELEMENT_SHELL/SOLID/BEAM/TSHELL` → part element definitions |
| `convertentities.h/.cxx` | Generic entity conversion utilities |
| `convertfrictions.h/.cxx` | Friction: `*DEFINE_FRICTION` → `/FRICTION` |
| `convertincludes.h/.cxx` | Include files: `*INCLUDE` → `/INCLUDE` |
| `convertinitialaxialforces.h/.cxx` | Initial axial force: `*INITIAL_AXIAL_FORCE` → bolt preload |
| `convertinitialstrains.h/.cxx` | Initial strain: `*INITIAL_STRAIN_*` → `/INISTA` |
| `convertinitialstresses.h/.cxx` | Initial stress: `*INITIAL_STRESS_*` → `/INISTA` |
| `convertinivels.h/.cxx` | Initial velocity: `*INITIAL_VELOCITY_*` → `/INIVEL` |
| `convertloads.h/.cxx` | Loads: `*LOAD_*` → `/CLOAD`, `/PLOAD`, `/GRAV` |
| `convertmats.h/.cxx` | Materials: `*MAT_*` → `/MAT` |
| `convertnodes.h/.cxx` | Nodes: `*NODE` → `/NODE` |
| `convertnodetransforms.h/.cxx` | Node coordinate transforms → `/SKEW` |
| `convertparameters.h/.cxx` | Parameters: `*PARAMETER` → `/PARAMETER` |
| `convertparts.h/.cxx` | Parts/sections/properties: `*PART`, `*SECTION_*` → `/PROP`, `/PART` |

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `_private/` | Internal converter utilities (rule map, rule parser, base classes) |

## Related Documentation

- `reader/source/dyna2rad/README.md` — parent directory
- `reader/source/dyna2rad/dyna2rad/_private/README.md` — internal utilities
