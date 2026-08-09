# Output Utility Tools (`starter/source/output/tools/`)

Low-level output utilities for the starter's binary write routines.

## Key Files

| File | Role |
|------|------|
| `ieee.cpp` | IEEE 754 floating-point utilities: convert between single/double precision and portable byte representations for binary output |
| `write_debug.F` | Debug output: dump internal data structures to text for diagnostic purposes |
| `wrtsqi.F` | Write integer array in sequence (packed binary) |
| `wrtsqr.F` | Write real array in sequence (packed binary) |

## Binary Format

`wrtsqi.F` and `wrtsqr.F` write arrays in OpenRadioss's packed binary format used in the restart file. The format uses platform-independent byte ordering (big-endian on some platforms, native on others). `ieee.cpp` handles the precision conversion when writing in a different precision than the current build (e.g., write single-precision restart from a double-precision build).

## Related Documentation

- `starter/source/output/README.md` — parent output directory
- `starter/source/restart/README.md` — restart file format
