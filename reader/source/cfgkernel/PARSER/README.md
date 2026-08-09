# CFG Parser (`reader/source/cfgkernel/PARSER/`)

Implements the Radioss deck parser layer: reads `.rad` / `_0001.rad` ASCII or CFG binary format and builds the in-memory entity model.

## Key Files

| File | Role |
|------|------|
| `mv_parser_base.cpp` / `.h` | Base parser class: tokeniser, keyword lookup, field extraction |
| `mv_read_base.cpp` / `.h` | Base reader class: file open/close, line iteration, include handling |

## Related Documentation

- `reader/source/cfgkernel/README.md` — parent directory
- `reader/source/cfgkernel/KERNEL/README.md` — CFG kernel that calls the parser
