# LS-DYNA Keyword Reader (`reader/source/io/model_readers/ls-dyna/keyword/`)

Implements the LS-DYNA `*KEYWORD` file parser: tokenises the ASCII deck,
dispatches each keyword block to the SDI model, and forwards warnings/errors
to the message handler.

## Key Files

| File | Role |
|------|------|
| `dynamain.h` / `dynamain.cxx` | Main entry point: opens `*KEYWORD` file, drives the per-block dispatch loop |
| `hwReaderMessage.h` / `hwReaderMessage.cxx` | Warning/error message formatter used during parse |

## Related Documentation

- `reader/source/io/model_readers/ls-dyna/README.md` — parent directory
- `reader/source/dyna2rad/dyna2rad/README.md` — post-parse LS-DYNA→Radioss converter
