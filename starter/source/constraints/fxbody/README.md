# Fixed (Prescribed-Motion) Body (`starter/source/constraints/fxbody/`)

Reads and initialises /FXBODY: rigid body with prescribed kinematic motion (position, velocity, acceleration as time functions).

## Key Files

| File | Role |
|------|------|
| `hm_read_fxb.F` | Parse /FXBODY card: node set, motion function IDs |
| `ini_fxbody.F` | Initialise fixed-body data structures and motion arrays |
| `fsigcini.F` | Initialise cylindrical FXBODY motion |
| `fsigpini.F` | Initialise planar FXBODY motion |
| `fsigsini.F` | Initialise spherical FXBODY motion |
| `fsigtini.F` | Initialise translational FXBODY motion |
| `fsigtrini.F` | Initialise translational-rotational FXBODY motion |
| `fxbelnum.F` | Enumerate elements belonging to the fixed body |
| `fxbgrav.F` | Apply gravity to fixed body |
| `fxbsini.F` | Fixed body SPMD initialisation |
| `fxbtagn.F` | Tag nodes belonging to fixed body |
| `hm_setfxrbyon.F` | Set fixed body "on" state from sensor |
| `modbufel.F` | Modify element buffer for fixed body |
| `moddepl.F` | Modify nodal displacement for prescribed motion |
| `ortho_normalization.F` | Orthonormalise rotation frame for FXBODY |
| `read_pch_file.F` | Read Nastran PCH file for prescribed motion data |

## Description

`/FXBODY` prescribes the entire motion of a node set (translation and rotation) via time functions, effectively making it behave as a rigid body with known kinematics. Unlike `/RBODY`, the motion is fully prescribed rather than computed from forces. Used for tooling, forming dies, and boundary motion imposition.

## Related Documentation

- `starter/source/constraints/general/rbody/README.md` — free rigid body
- `engine/source/constraints/README.md` — engine constraint enforcement
