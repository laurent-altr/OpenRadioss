# Engine QA Print (`engine/source/output/qaprint/`)

Writes QA (Quality Assurance) summaries at the start of the engine run: echoes key control parameters and model statistics.

## Key Files

| File | Role |
|------|------|
| `eng_qaprint_animinput.F` | Echo animation output settings to `.out` |
| `eng_qaprint_driver.F` | Master QA print dispatcher: call all sub-printers |
| `eng_qaprint_dtinput.F` | Echo time step control parameters |
| `eng_qaprint_generalcontrolsinput.F` | Echo general control card settings |
| `ieee.cpp` | IEEE floating-point environment settings and reporting |
| `redsqi.F` / `redsqr.F` | Read integer/real QA quantities from restart |
| `wrtsqi.F` / `wrtsqr.F` | Write integer/real QA quantities to restart |

## Related Documentation

- `engine/source/output/README.md` — parent output directory
- `starter/source/output/qaprint/README.md` — starter QA print
