# Output File Headers (`starter/source/output/outfile/`)

Writes the main `.out` file header and title sections.

## Key Files

| File | Role |
|------|------|
| `titre1.F` | Write the OpenRadioss title banner and run identification to `.out` |
| `titre2.F` | Write the model title and unit system header |
| `titre3.F` | Write the analysis type and control parameter summary |
| `printgroup.F` | Generic group-printing utility for structured table output |

## Output File Structure

The `.out` file is the primary human-readable run log. Its header (written by this directory) contains:
1. OpenRadioss version, build date, platform
2. Run title (from `/TITLE` in the input deck)
3. Unit system (from `/UNIT`)
4. Key control parameters (end time, output intervals, solver type)

After the header, the QA print tables (`starter/source/output/qaprint/`) provide the model echo.

## Related Documentation

- `starter/source/output/README.md` — parent output directory
- `starter/source/output/qaprint/README.md` — QA model echo tables
