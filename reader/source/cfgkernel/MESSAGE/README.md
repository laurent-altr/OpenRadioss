# CFG Message System (`reader/source/cfgkernel/MESSAGE/`)

Warning and error message management for the CFG reader: message state tracking, message type definitions, and utility functions.

## Key Files

| File | Role |
|------|------|
| `msg_manager.cpp` / `.h` | Message manager: collect, filter, and output warnings/errors |
| `msg_state.h` | Message state enumeration (OK, WARNING, ERROR, FATAL) |
| `msg_types.h` | Message type identifiers |
| `msg_utils.cpp` | Message formatting and output utilities |

## Related Documentation

- `reader/source/cfgkernel/README.md` — parent directory
