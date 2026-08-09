# Starter Checksum Output (`starter/source/output/checksum/`)

Computes and writes model-level checksums at the end of the starter run for regression testing and input verification.

## Key Files

| File | Role |
|------|------|
| `checksum_check.F90` | Validate checksum against stored reference value |
| `checksum_list.cpp` | Manage list of quantities included in checksum |
| `checksum_model.cpp` | Compute checksum over the full model data arrays |
| `checksum_option.F90` | Read `/CHECKSUM` option from input |
| `checksum_output_files.cpp` | Write checksum to output files |

## Purpose

The starter checksum hashes the fully-built model arrays (node coordinates, element connectivity, material parameters, boundary conditions) into a single scalar value. This value is written to the `_0001.out` file and can be compared against a reference to detect:
- Input file corruption or round-trip errors
- Changes in starter parsing logic during code development
- Platform-specific differences in parsing

## Related Documentation

- `starter/source/output/README.md` — starter output
- `common_source/output/checksum/README.md` — engine-side checksum
