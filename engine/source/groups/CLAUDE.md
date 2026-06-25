# engine/source/groups/

## Purpose
Engine-side group and set allocation/initialization helpers. Allocates and
initializes the in-memory structures for element groups, surface groups, line
groups, and subsets read from the restart file.

## Files

| File | Role |
|------|------|
| `alloc_group_str.F` | Allocates `GROUP_STR` (element group structure arrays) |
| `alloc_line_str.F` | Allocates line (1D element) group structures |
| `alloc_surf_str.F` | Allocates surface group structures |
| `alloc_subset_str.F` | Allocates subset (node/element set) structures |
| `group_ini.F` | Initializes element group arrays from restart file data |
| `line_ini.F` | Initializes line element group data |
| `surf_ini.F` | Initializes surface element group data |
| `subset_ini.F` | Initializes subset data |

## Context
These routines are called once during engine startup (from `LECSTAT`/`LECTUR`
in `engine/source/input/`) to rebuild the group arrays from the restart file.
After initialization, the groups are read-only throughout the time loop.

The group descriptor array `IPARG(NPARG, NGROUP)` is filled by the Starter
(`sgrtails.F`, `cgrtails.F`, etc.) and read here; see
`doc/GROUPS_AND_CONNECTIVITY_documentation.md` for `IPARG` slot meanings.

## Dependencies
- Called by: `engine/source/input/lecstat.F`, `lectur.F`
- Uses: `GROUPDEF_MOD`, `SETDEF_MOD` from `common_source/modules/`
