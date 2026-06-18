# Model Reader Utilities (`reader/source/io/model_readers/util/`)

Shared warning and error message infrastructure used by both the Radioss
and LS-DYNA model readers.

## Key Files

| File | Role |
|------|------|
| `hwReaderMessage.h` / `hwReaderMessage.cxx` | `hwReaderMessage` (single message: id, title, description, solution), `hwReaderMessagePool` (static registry of message templates), `hwReaderMessageList` (runtime ordered list of emitted messages) |

## Related Documentation

- `reader/source/io/model_readers/README.md` — parent directory
- `reader/source/cfgkernel/MESSAGE/README.md` — lower-level message manager
