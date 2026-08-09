# Engine Output Control (`starter/source/general_controls/engine/`)

Reads engine output control cards (/ANIM, /H3D/ANIM, /TH, /OUTP, /PRINT) that configure what results are written by the engine and at what frequency.

## Key Files

| File | Role |
|------|------|
| `hm_read_anim.F` | Parse /ANIM and /H3D/ANIM: animation output variables and frequency |
| `hm_read_dt.F` | Parse /DT: output time-step list for animation and time-history |
| `hm_read_outp.F` | Parse /OUTP: time-history output (TH) variable and group selectors |
| `read_engine_driver.F` | Read engine run-control parameters (termination time, DT scale) |
| `hm_read_ioflag.F` | Read I/O format flags: binary/ASCII, endianness, precision |

## Description

`hm_read_anim.F` processes the /ANIM family of cards, registering which nodal and element result variables (stress, strain, velocity, plastic strain, etc.) are to be written to animation files. `hm_read_outp.F` registers time-history groups (nodes, elements, sets) and the output variables for each. `hm_read_dt.F` builds the output time list that controls when the engine writes snapshots. All parameters are written to the restart file for the engine to consume.

## Related Documentation

- `starter/source/general_controls/README.md` — parent directory
- `engine/source/output/README.md` — engine output subsystem
- `engine/source/output/anim/README.md` — animation writer
- `engine/source/output/th/README.md` — time-history writer
