# Element Activation (`starter/source/tools/activ/`)

Reads `/ACTIV` (element activation) definitions — sensors that activate or deactivate element groups during the simulation.

## Key Files

| File | Role |
|------|------|
| `hm_read_activ.F` | HM binary reader for `/ACTIV` keyword |

## Element Activation

`/ACTIV` allows elements to be initially inactive (zero stiffness, zero mass) and activated when a sensor triggers. This is used for:
- Staged construction simulations (activate structural elements at specified times)
- Airbag membrane activation before inflation
- Void material that activates when a crack front reaches it (with XFEM)

The starter reads the activation condition (sensor ID, element group to activate/deactivate) and writes the mapping to the restart file. The engine evaluates the sensor and activates elements accordingly.

## Related Documentation

- `starter/source/tools/README.md` — parent tools directory
- `starter/source/tools/sensor/README.md` — sensor definitions used as activation triggers
