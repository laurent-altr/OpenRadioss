# Sensor Initialisation (`starter/source/tools/sensor/`)

Reads and initialises `/SENSOR` definitions. Sensors monitor simulation state and trigger events (element activation, airbag deployment, output).

## Key Files

| File | Role |
|------|------|
| `inisen.F` | Main sensor initialisation: allocate sensor arrays, call per-type initialisation |
| `iniparsen.F` | Initialise parent-sensor relationships (logical AND/OR chains) |
| `sensor_tab_init.F` | Initialise sensor trigger tables and cross-references |
| `sort_logical_sensors.F` | Sort logical sensors (AND/OR) in evaluation order |
| `write_sensors.F` | Write sensor initialisation data to restart file |
| `sensor_user_alloc.F` | Allocate user-defined sensor data structures |
| `sensor_user_init.F` | Initialise user-defined sensor callbacks |
| `sensor_user_convert_local_id.F` | Convert global IDs to local (rank-local) for SPMD sensors |
| `set_u_sens_spmd_node_list.F` | Build per-rank node lists for displacement/velocity sensors |
| `read_sensor_acc.F` | Read `/SENSOR/ACCEL` — acceleration-based trigger |
| `read_sensor_and.F` | Read `/SENSOR/AND` — logical AND of two sensors |
| `read_sensor_contact.F` | Read `/SENSOR/INTER` — contact force trigger |
| `read_sensor_disp.F` | Read `/SENSOR/DISP` — displacement trigger |
| `read_sensor_dist_surf.F` | Read `/SENSOR/DIST_SURF` — distance-to-surface trigger |
| `read_sensor_energy.F` | Read `/SENSOR/ENERGY` — energy threshold trigger |
| `read_sensor_gauge.F` | Read `/SENSOR/GAUGE` — gauge (output point) trigger |
| `read_sensor_hic.F` | Read `/SENSOR/HIC` — Head Injury Criterion trigger |
| `read_sensor_nic.F` | Read `/SENSOR/NIC` — Neck Injury Criterion trigger |
| `read_sensor_not.F` | Read `/SENSOR/NOT` — logical NOT |
| `read_sensor_or.F` | Read `/SENSOR/OR` — logical OR |
| `read_sensor_rbody.F` | Read `/SENSOR/RBODY` — rigid body velocity/displacement trigger |
| `read_sensor_rwall.F` | Read `/SENSOR/RWALL` — rigid wall force trigger |
| `read_sensor_sect.F` | Read `/SENSOR/SECT` — section force trigger |
| `read_sensor_sens.F` | Read `/SENSOR/SENS` — sensor-of-sensors chaining |
| `read_sensor_temp.F` | Read `/SENSOR/TEMP` — temperature trigger |
| `read_sensor_time.F` | Read `/SENSOR/TIME` — time-based trigger |
| `read_sensor_user.F` | Read `/SENSOR/USER` — user-defined sensor |
| `read_sensor_vel.F` | Read `/SENSOR/VEL` — velocity trigger |
| `read_sensor_work.F` | Read `/SENSOR/WORK` — work (energy) trigger |
| `hm_read_sensors.F` | HM binary reader for all sensor types |
| `hm_read_sensor_python.F90` | Python-defined sensor via HM reader path |

## Sensor Evaluation (Engine)

The starter only reads and initialises sensors. The engine evaluates them each time step:
1. Compute monitored quantity (nodal displacement, contact force, time, etc.)
2. Compare against trigger threshold (or evaluate logical combination)
3. If triggered: activate elements, deploy airbag, start output, etc.

## Related Documentation

- `engine/source/tools/README.md` — engine sensor evaluation (`rsens_nic.F`)
- `starter/source/tools/README.md` — parent tools directory
