# Starter REFSTA (Reference State) (`starter/source/loads/reference_state/refsta/`)

Reads `/REFSTA` global reference state: a coordinate transformation applied to the initial nodal positions before the simulation starts (virtual reference configuration).

## Key Files

| File | Role |
|------|------|
| `hm_read_refsta.F` | Read `/REFSTA` card: translation and rotation of the global reference frame |
| `lecrefsta.F` | Legacy reader for reference state |

## Description

`/REFSTA` allows the initial mesh to be defined in a coordinate system different from the simulation coordinate system. The starter applies the specified transformation (translation vector + rotation matrix) to all nodal coordinates before writing the restart file. Used for models assembled from multiple sub-models in different local coordinate systems.

## Related Documentation

- `starter/source/loads/reference_state/README.md` — parent directory
