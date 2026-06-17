# XFEM Initialisation (`starter/source/elements/xfem/`)

Initialises eXtended Finite Element Method (XFEM) crack propagation data structures.

## Key Files

| File | Role |
|------|------|
| `allocxfem.F` | Allocate XFEM-specific arrays (level-set fields, enrichment DOF tables) |
| `precrkxfem.F` | Pre-process crack geometry: identify initially cracked elements |
| `preinicrk3N.F` | Initialise crack front for 3-node shell elements |
| `preinicrk4N.F` | Initialise crack front for 4-node shell elements |
| `lslocal.F` | Compute local level-set values at nodes relative to crack surface |
| `cbufxfe.F` | Fill XFEM element buffer with enrichment data |
| `fillcne_xfem.F` | Fill crack normal and enriched node tables |
| `iedge_xfem.F` | Identify element edges intersected by the crack front |
| `pretag_xfem.F` | Tag elements as enriched/partially enriched/unenriched |
| `thick_ilev.F` | Compute thickness integration levels for cracked shell elements |
| `inicrkfill.F` | Write initial crack data to restart file |
| `xfem_crack_init.F` | Top-level XFEM initialisation driver |

## XFEM in OpenRadioss

XFEM models crack propagation without remeshing. The crack is represented as a level-set field (signed distance to crack surface). Elements cut by the crack are enriched with Heaviside / branch functions.

The starter is responsible for:
1. Reading initial crack geometry (`/XFEM`, `/INICRK`)
2. Tagging cut and tip elements (`pretag_xfem.F`)
3. Computing level-set values at all nodes (`lslocal.F`)
4. Identifying enriched DOFs (`fillcne_xfem.F`)
5. Writing enrichment tables to the restart file

The engine then propagates the crack each time step using stress-intensity factor criteria.

## Related Documentation

- `starter/source/elements/README.md` — parent directory overview
- `starter/source/initial_conditions/README.md` — `/INICRK` initial crack conditions
- `engine/source/elements/README.md` — engine XFEM force computation and propagation
