# Checksum Output (`common_source/output/checksum/`)

Computes and writes determinism-checking checksums for regression testing of OpenRadioss runs.

## Key Files

| File | Role |
|------|------|
| `checksum.cpp` | C++ checksum computation: CRC/hash of selected output quantities |

## Purpose

`checksum.cpp` computes a hash (typically CRC32 or similar) of key result quantities (nodal displacements, element energies, contact forces) at specified output steps. The checksum is written to the `.out` run-log file. Two runs on the same platform with identical inputs must produce identical checksums, verifying:

- Bit-exact reproducibility across MPI decompositions
- No floating-point ordering issues in reductions
- Regression detection after code changes

The checksum system is part of the OpenRadioss CI regression suite. See `common_source/modules/output/README.md` for the `checksum_mod` module that stores configuration.

## Related Documentation

- `common_source/output/README.md` — parent output directory
- `engine/source/output/th/README.md` — TH checksum (`thchecksum.F90`)
