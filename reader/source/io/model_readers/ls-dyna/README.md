# LS-DYNA Model Reader (`reader/source/io/model_readers/ls-dyna/`)

Reads LS-DYNA keyword format into the in-memory SDI model (used by the DYNA2RAD conversion pipeline).

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `keyword/` | Keyword-by-keyword LS-DYNA reader |

## Key Files

| File | Role |
|------|------|
| `dynamain.h` / `dynamain.cxx` | Main LS-DYNA reader entry point: `*KEYWORD` file parsing |
| `hwReaderMessage.h` / `hwReaderMessage.cxx` | Reader warning/error message handler |

## Related Documentation

- `reader/source/io/model_readers/README.md` — parent directory
- `reader/source/dyna2rad/README.md` — DYNA2RAD converter that uses this reader
