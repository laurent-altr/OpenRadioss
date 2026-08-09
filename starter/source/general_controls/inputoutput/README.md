# Input/Output Flags (`starter/source/general_controls/inputoutput/`)

Reads /IOFLAG and related cards that control starter input parsing flags and output file formatting.

## Key Files

| File | Role |
|------|------|
| `hm_read_ioflag.F` | Parse /IOFLAG: verbosity, error handling, unit-system code |

## Description

`hm_read_ioflag.F` reads the `/IOFLAG` card which controls starter diagnostic output (warning level, missing-data behaviour), and the default unit system code. The flags are stored in global control modules and influence how subsequent cards are parsed and reported.

## Related Documentation

- `starter/source/general_controls/README.md` — parent directory
- `starter/source/general_controls/computation/README.md` — computation control (unit reader)
