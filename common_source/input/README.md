# Common Input Utilities (`common_source/input/`)

Shared input parsing utilities used by both starter and engine keyword readers.

## Key Files

| File | Contents |
|------|---------|
| `nvar.F` | `NVAR` — count variables: utility subroutine to count how many values appear on an input line (used for free-format parsing) |

## Purpose

`nvar.F` supports the free-format input parser. When a keyword line can have a variable number of arguments (e.g., a `/GRNOD` with an arbitrary-length node list), `NVAR` counts the tokens on the current line so the reader can allocate the right array size before reading values.

This is a shared utility used by both `lectur.F` in the starter and `freform.F` in the engine's keyword reader.

## Related Documentation

- `starter/source/starter/README.md` — `lectur.F` uses this for parsing
- `engine/source/input/README.md` — engine keyword reader
- `common_source/tools/input_output/README.md` — binary I/O utilities (separate from text parsing)
