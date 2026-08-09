# Message System (`starter/source/output/message/`)

Manages user-readable warning and error messages for the starter.

## Key Files

| File | Role |
|------|------|
| `message.F` | Main message dispatch: format and write a message by ID |
| `build_msg.F` | Build message string from template and variable arguments |
| `inimsg.F` | Initialise message system: load message templates from file |
| `read_msgfile.F` | Read message template file (`starter_message_description.txt`) |
| `stock_msg.F` | Store a message in the output buffer (for deferred printing) |
| `summsg.F` | Print message summary at end of starter run (count of warnings/errors) |
| `fredec2im.F` | Free-format decimal integer formatting |
| `fredec3m.F` | Free-format decimal floating-point formatting |
| `starter_message_description.txt` | Text file: message templates indexed by message ID |

## Message System Design

Messages are defined externally in `starter_message_description.txt` rather than hardcoded in source. Each message has:
- A numeric ID (referenced in Fortran source)
- A severity (INFO / WARNING / ERROR)
- A template string with `%d`, `%f`, `%s` placeholders

`message.F` looks up the template by ID, substitutes arguments, and writes to `.out`. This design:
- Allows translating messages without recompiling
- Centralises all message text for review
- Makes it easy to grep for a specific message ID in the source

## Related Documentation

- `starter/source/output/README.md` — parent output directory
- `engine/source/system/README.md` — `ARRET` (fatal error — different mechanism)
