# Rad2Rad Coupling (`starter/source/coupling/rad2rad/`)

Reads and initialises /RAD2RAD coupling: connects two Radioss instances sharing boundary nodes for co-simulation.

## Key Files

| File | Role |
|------|------|
| `r2r_input.F` | Main rad2rad input reader |
| `r2r_prelec.F` | Pre-read: count rad2rad definitions |
| `r2r_prelec_name.F` | Pre-read by name for rad2rad blocks |
| `r2r_count.F` | Count shared nodes and elements |
| `r2r_group.F` | Build rad2rad node groups |
| `r2r_fork.F` | Fork rad2rad communicators for parallel launch |
| `r2r_domdec.F` | Domain decomposition for rad2rad boundary |
| `r2r_split.F` | Split boundary data across SPMD domains |
| `r2r_check.F` | Validate rad2rad configuration |
| `r2r_clean_inter.F` | Remove internal contact at rad2rad interface |
| `r2r_speedup.F` | Compute DT scaling for rad2rad coupling |
| `r2r_void.F` | Handle void (absent) rad2rad definitions |
| `lecextlnk.F` | Read external link (rad2rad) from restart |
| `new_link.F` | Create new rad2rad link entry |
| `routines_r2r.F` | Utility routines for rad2rad |
| `tagelem_r2r.F` | Tag elements at rad2rad boundary |
| `tagint_r2r.F` | Tag interfaces at rad2rad boundary |
| `tagnod_r2r.F` | Tag nodes at rad2rad boundary |
| `tagnod_r2r_nl.F` | Tag non-linear rad2rad boundary nodes |

## Description

Rad2Rad co-simulation splits a model across two Radioss engine processes. Each engine runs its own domain, and they exchange boundary forces and velocities at each time step. The starter reads the coupling definition, partitions nodes between the two domains, and writes coupling tables to the restart files of both instances.

## Related Documentation

- `starter/source/coupling/README.md` — parent coupling directory
- `engine/source/coupling/README.md` — engine-side coupling (preCICE, CWIPI, rad2rad)
