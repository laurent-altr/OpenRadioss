# Input/Output Utilities (`common_source/tools/input_output/`)

Low-level text and binary I/O helpers shared between starter and engine.

## Key Files

| File | Role |
|------|------|
| `read_db.F` | Read database records: structured binary read from restart/database files |
| `write_db.F` | Write database records: structured binary write |
| `write_routines.c` | C-level write helpers with buffered I/O and large-file (>2GB) support |

## Design

These routines provide a thin abstraction over Fortran `READ`/`WRITE` and C `fwrite`:
- `read_db.F` / `write_db.F` handle Fortran record semantics and byte-order portability
- `write_routines.c` handles the large-file case (files > 2 GB) where standard Fortran I/O may fail due to 32-bit offset limitations

For the main binary restart format, see `engine/source/input/README.md` and `starter/source/restart/README.md`. These utilities provide the underlying primitives those higher-level modules use.

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `common_source/comm/README.md` — compress/decompress and portable binary I/O
- `starter/source/restart/README.md` — restart file write
- `engine/source/input/README.md` — restart file read
