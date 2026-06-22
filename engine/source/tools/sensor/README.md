# Engine Sensor Evaluation (`engine/source/tools/sensor/`)

Evaluates sensor states each time step: sensors monitor physical quantities and trigger events (airbag deployment, element activation, load switching).

## Key Files

| File | Role |
|------|------|
| `sensor_acc.F` | Acceleration sensor: evaluate nodal/group acceleration vs. threshold |
| `rsens_nic.F` | NIC (Neck Injury Criterion) sensor computation |
| `dist_node_plane_3n.F` | Distance from node to 3-node plane (geometric sensor) |
| `dist_node_seg3n.F` | Distance from node to 3-node segment |
| `dist_node_seg4n.F` | Distance from node to 4-node segment |

## Architecture

Each step the engine calls the sensor evaluation loop (in `engine/source/general_controls/`) which dispatches to per-sensor-type evaluation routines. Sensors output a binary `active/inactive` state plus a scalar value. Upon activation, sensors trigger downstream events via the sensor dependency table. The geometric sensor routines (`dist_node_*`) compute signed distances needed for penetration-based and contact-based sensor types.

## Related Documentation

- `engine/source/tools/README.md` — parent tools directory
- `starter/source/tools/sensor/README.md` — sensor definition in starter
