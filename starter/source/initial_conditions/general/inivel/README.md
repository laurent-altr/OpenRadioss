# Initial Velocity (`starter/source/initial_conditions/general/inivel/`)

Reads and applies /INIVEL initial velocity conditions to node groups, parts, or the full model.

## Key Files

| File | Role |
|------|------|
| `hm_preread_inivel.F90` | Pre-read: count /INIVEL blocks, allocate arrays |
| `hm_read_inivel.F` | Main reader: parse translational + rotational velocity, node/part selectors |
| `inivel.F` | Apply initial velocities to the nodal velocity array (VX/VY/VZ) |

## Description

`hm_read_inivel.F` parses the `/INIVEL` card variants including rigid body initial velocity (`/INIVEL/RBODY`), FVM fluid velocity (`/INIVEL/FVM`), and local-frame velocity. `inivel.F` iterates over the selected node set and writes the velocity components into the global nodal arrays before the first engine step.

## Related Documentation

- `starter/source/initial_conditions/general/README.md` — parent directory
- `engine/source/loads/general/inivel/README.md` — engine-side initial velocity application
