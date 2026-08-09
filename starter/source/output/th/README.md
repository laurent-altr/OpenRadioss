# Time History Output Setup (`starter/source/output/th/`)

Reads and initialises `/TH` (time history) output requests. Time history records nodal or element quantities at high temporal resolution (every step or every few steps).

## Key Files

| File | Role |
|------|------|
| `thprin.F` | Main TH initialisation: process all `/TH` requests, build output lists |
| `thpinit.F` | Initialise TH output buffer headers |
| `th_titles.F90` | Write TH variable title strings (column headers in the `.th` file) |
| `write_thnms1.F90` | Write TH node/element name lists to restart |
| `write_thnms1_titles.F90` | Write titles with variable names |
| `write_thnms1_empty_titles.F90` | Write placeholder titles (for not-yet-known variables) |
| `hord.F` / `hord3.F` | Sort TH output lists by entity ID |
| `thskewc.F` | Transform TH output to local skew frame |
| `th_surf_load_pressure.F` | TH output for surface load pressure |
| `hm_read_th.F` | HM binary reader for `/TH` definitions |
| `hm_read_prethgrou.F` | HM pre-read for TH group definitions |
| `hm_read_thgrou.F` | HM reader for TH node/element groups |
| `hm_read_thgrki.F` | HM reader for TH kinematic groups (node velocity, displacement) |
| `hm_read_thgrki_rbody.F` | HM reader for TH rigid body kinematic output |
| `hm_read_thgrne.F` | HM reader for TH near-field output |
| `hm_read_thgrns.F` | HM reader for TH nodal stress output |
| `hm_read_thgrpa.F` | HM reader for TH part output |
| `hm_read_thgrsens.F` | HM reader for TH sensor output |
| `hm_read_thgrsurf.F` | HM reader for TH surface output |
| `hm_read_thvarc.F` | HM reader for TH variable codes |
| `hm_thgrki_vent.F` | HM reader for TH airbag vent output |
| `hm_thvarent.F` | HM reader for TH entity variables |
| `hm_read_thchecksum.F90` | HM reader for TH checksum definitions |

## TH Output Variables

Common TH variables:
- Nodal: displacement, velocity, acceleration, temperature
- Element: stress, strain, plastic strain, damage, density
- Rigid body: force, torque, velocity, position
- Section: force resultant through a section cut
- Sensor: triggered state, monitored quantity value
- Airbag: pressure, volume, temperature, vent flow rate

## Related Documentation

- `starter/source/output/README.md` — parent output directory
- `engine/source/output/README.md` — engine TH output (`hist1.F`, `hist2.F`)
