# Engine Input Subsystem

This subsystem reads the restart file and any supplementary input at engine startup. Unlike the starter (which parses the full keyword deck), the engine reads binary restart data and a small number of engine-level keywords.

## What the Engine Reads

At startup, the engine reads:
1. **Binary restart file** (`_<runN>.rad`) — the full model state written by the starter or a previous engine run
2. **Engine keyword input** — a small set of keywords that can override restart parameters (e.g. `/STOP`, `/PRINT`, `/ANIM/`, `/TH/`)
3. **DYNAIN file** (optional) — deformed geometry from a previous run for forming/springback chains

## Key Files

| File | Role |
|------|------|
| `freform.F` | Top-level engine input reader — reads restart file and engine keywords |
| `freabf.F` | Read element buffer (ABF) data from restart |
| `frebcs.F` | Read boundary condition data from restart |
| `frecpl.F` | Read coupling interface data from restart |
| `fredamp.F` | Read damping parameters from restart |
| `fredebug.F` | Read debug output parameters |
| `fredec_8key_i.F` | Read 8-byte integer arrays from restart (double-precision index) |
| `fredli.F`, `fredli7.F` | Read load curve data from restart |
| `fredynain.F` | Read DYNAIN deformed geometry file |
| `freeig.F` | Read eigenvalue / modal data from restart |
| `freflw.F` | Read fluid / ALE data from restart |
| `fraleonoff.F` | Read ALE on/off flags |
| `fralnk.F` | Read link (kinematic constraint) data from restart |
| `freleonoff.F` | Read element on/off (eroded element) flags |
| `errmsg.F` | Engine-level error message formatting |

(Additional `fre*.F` files read other data categories from restart.)

## Restart File Format

The binary restart file uses a block structure: each block has a tag identifying the data type followed by the array data. The engine reads blocks sequentially using the `fre*.F` family of routines, each handling a specific data category. The format is versioned — blocks not recognised by the current version are skipped.

## Engine-Level Keyword Override

After reading the restart, the engine processes any additional input passed on the command line (e.g. an engine input file). This allows changing output frequency, adding `/STOP` conditions, or enabling debug output without rerunning the starter.

## DYNAIN Read (`fredynain.F`)

When a forming simulation produces a DYNAIN file, the next simulation (springback) starts by reading the deformed geometry and residual stresses from it. `fredynain.F` overlays these onto the restart data before the time loop begins.

## Related Documentation

- `starter/source/README.md` — the starter writes the restart file that this subsystem reads
- `engine/source/output/restart/` — engine restart writer (for run continuation)
- `engine/source/README.md` — `resol_init.F` calls the input routines at startup
