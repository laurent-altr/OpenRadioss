# Output Parameter Reader (`starter/source/output/outp/`)

Reads general output control settings.

## Key Files

| File | Role |
|------|------|
| `desout.F` | Read `/OUTP` keyword: set output file destination, precision, and format options |

## Output Parameters

`/OUTP` controls miscellaneous output parameters:
- Output file path prefix
- Binary output precision (single or double for restart/animation)
- Animation file format (legacy or H3D)
- Verbose/quiet output level for `.out` file
- Enable/disable specific output types

## Related Documentation

- `starter/source/output/README.md` — parent output directory
