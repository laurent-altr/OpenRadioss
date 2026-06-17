# QA Print Tables (`starter/source/output/qaprint/`)

Writes per-keyword summary tables to the `.out` file — the human-readable echo of the input deck after parsing. Each `st_qaprint_*.F` file handles one subsystem.

## Key Files

| File | Subsystem Covered |
|------|------------------|
| `st_qaprint_driver.F` | Driver: calls all per-subsystem QA print routines |
| `st_qaprint_element.F` | Element summary tables (count, type, property, material) |
| `st_qaprint_constraints.F` | Rigid bodies, RBE2, RBE3, rigid walls, rigid links |
| `st_qaprint_groups.F` | Node/element group summary |
| `st_qaprint_ebcs.F` | Extended boundary conditions |
| `st_qaprint_general_controls.F` | Run control, output, print, animation settings |
| `st_qaprint_composite_options.F` | Composite stack/drape summary |
| `st_qaprint_friction.F` | Friction model parameters |
| `st_qaprint_ale_options_driver.F` | ALE/Euler setup summary |
| `st_qaprint_admesh.F` | Adaptive mesh refinement regions |
| `st_qaprint_clusters.F` | Cluster definitions |
| `st_qaprint_dfs_detonators.F` | Detonator definitions for HE models |
| `st_qaprint_dfs_lasers.F` | Laser load definitions |
| `st_qaprint_initial_conditions.F` | Initial velocity, initial stress, initial crack |
| `st_qaprint_initial_state.F` | Initial state (from DYNAIN/INISTATE) |
| `printgroup.F` | Generic group-printing utility used by multiple qaprint routines |

## Purpose

The QA print tables serve as the model echo in the `.out` file — the analyst can verify that all input was read correctly without re-reading the original keyword file. Each table shows:
- Number of entities read
- Key parameters (IDs, magnitudes, function references)
- Warnings about unusual or potentially erroneous values

This is the primary mechanism for catching misread or misinterpreted input before the run starts.

## Related Documentation

- `starter/source/output/README.md` — parent output directory
