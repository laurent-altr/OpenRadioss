# Engine Output Tools (`engine/source/output/tools/`)

Shared utility routines used by multiple output modules.

## Key Files

| File | Role |
|------|------|
| `fredec2i.F` | Decode integer from output format |
| `fredec3.F` | Decode floating-point from output format |

## Description

These low-level format conversion utilities are shared across animation, TH, and state output modules. They handle the specific binary encoding used in legacy Radioss output formats (state and animation files) where numbers are packed in a compact binary format distinct from standard IEEE754. Needed for backward-compatible reading of old output files.

## Related Documentation

- `engine/source/output/README.md` — parent output directory
