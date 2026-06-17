# Added Mass (`starter/source/tools/admas/`)

Reads `/ADMAS` (added mass) definitions — point masses and inertias applied to nodes.

## Key Files

| File | Role |
|------|------|
| `hm_read_admas.F` | HM binary reader for `/ADMAS` keyword |
| `add_mass_stat.F` | Statistics: print total added mass and CG shift due to `/ADMAS` |
| `addmaspart.F` | Distribute added mass across a part's nodes |
| `addmast10.F` | Added mass for TYPE10 nodes (nodes with rotational DOF) |
| `surfmas.F` | Compute surface mass: mass per unit area for shell elements (`/SURFC`) |

## Added Mass Types

| Keyword | Applies to | Description |
|---------|-----------|-------------|
| `/ADMAS/NODE` | Single node | Point mass at a node |
| `/ADMAS/GRNOD` | Node group | Distributed mass over a node group |
| `/ADMAS/PART` | Part | Total mass distributed over all part nodes |
| `/ADMAS/RBODY` | Rigid body | Added mass/inertia to a rigid body CG |

Added mass affects the global mass matrix (diagonal) and therefore the critical time step. `add_mass_stat.F` prints a warning if the added mass is large relative to the structural mass.

## Related Documentation

- `starter/source/tools/README.md` — parent tools directory
- `engine/source/ams/README.md` — selective mass scaling (different from `/ADMAS`)
