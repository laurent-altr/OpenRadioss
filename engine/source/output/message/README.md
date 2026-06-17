# Engine Message Output (`engine/source/output/message/`)

Manages run-time warning and error messages written to the `.out` log file and standard output.

## Key Files

| File | Role |
|------|------|
| `build_msg.F` | Build formatted message strings from code + arguments |
| `engine_message_description.txt` | Text database of message codes → human-readable descriptions |
| `ini_msg.F` | Initialise message system at engine startup |
| `message.F` | Main message dispatch: route messages to log/stderr |
| `read_msgfile.F` | Read `engine_message_description.txt` at startup |
| `stock_msg.F` | Buffer message for batch output |

## Architecture

Messages are identified by numeric codes. `engine_message_description.txt` maps codes to format strings. `build_msg.F` fills format arguments, `message.F` decides whether to print immediately, buffer, or suppress repeated messages. Fatal messages call `ARRET()` after printing.

## Related Documentation

- `engine/source/output/README.md` — parent output directory
- `starter/source/output/message/README.md` — starter message system
