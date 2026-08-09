# Solver Interface — Messages (`reader/source/solver_interface/source/messages/`)

Fortran-callable wrappers that retrieve reader warning/error messages
from the CFG/SDI message store and pass them back to the Fortran starter.

## Key Files

| File | Role |
|------|------|
| `cpp_get_message.cpp` | `cpp_get_message_` — fetch one Radioss reader message by index: title, description, solution, file/line |
| `cpp_get_message_radioss.cpp` | Radioss-format variant of the message fetcher |
| `cpp_get_message_dyna.cpp` | LS-DYNA reader message fetcher |
| `cpp_get_message_number.cpp` | Return total count of reader messages |
| `cpp_get_message_number_radioss.cpp` | Count Radioss reader messages |
| `cpp_get_message_number_dyna.cpp` | Count LS-DYNA reader messages |

## Related Documentation

- `reader/source/solver_interface/source/README.md` — parent directory
- `reader/source/cfgkernel/MESSAGE/README.md` — underlying message store
