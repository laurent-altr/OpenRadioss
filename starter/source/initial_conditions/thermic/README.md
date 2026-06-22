# Initial Thermal Conditions (`starter/source/initial_conditions/thermic/`)

Reads /INITEMP to set per-node or per-part initial temperature fields.

## Key Files

| File | Role |
|------|------|
| `hm_read_initemp.F` | Parse /INITEMP card: temperature value or function ID, node/part set selector |

## Description

`hm_read_initemp.F` reads the `/INITEMP` block and populates the nodal temperature array `T0(n)`. Temperatures can be uniform (scalar), function-of-node-coordinates (via function ID), or imported from a field. The resulting array is written to the restart file and used by the engine thermal solver and temperature-dependent material laws.

## Related Documentation

- `starter/source/initial_conditions/README.md` — parent directory
- `engine/source/constraints/thermic/README.md` — engine-side thermal boundary conditions
- `starter/source/loads/thermic/README.md` — thermal load reader
