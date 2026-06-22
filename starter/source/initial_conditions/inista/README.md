# Initial State from Y-file (`starter/source/initial_conditions/inista/`)

Reads per-element initial stress/strain state from an external Y-file (DYNAIN or user-written state file) via /INISTA.

## Key Files

| File | Role |
|------|------|
| `hm_read_inista.F` | Parse /INISTA card: Y-file path, element set, field selectors |
| `lec_inistate_yfile.F` | Read Y-file: parse element stress, plastic strain, history variables |
| `yctrl.F` | Y-file format control: detect version, endianness, field layout |

## Description

`/INISTA` maps the output of a previous simulation (written as a Y-file by DYNAIN or `genoutp`) into the initial stress and internal-variable arrays of the current model. `lec_inistate_yfile.F` reads the binary Y-file element-by-element, matching by element ID, and distributes stress tensors and history variables to integration points. `yctrl.F` handles format compatibility between different Radioss versions.

## Related Documentation

- `starter/source/initial_conditions/README.md` — parent directory
- `starter/source/initial_conditions/inimap/README.md` — field mapping from external file
- `engine/source/output/dynain/README.md` — DYNAIN / Y-file writer
