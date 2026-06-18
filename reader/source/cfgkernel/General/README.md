# CFG General Structures (`reader/source/cfgkernel/General/`)

C++ header-only definitions of the fundamental data structures used throughout the CFG kernel: containers, descriptors, expressions, and type identifiers.

## Key Files

| File | Role |
|------|------|
| `Structure_container.h` | Generic container (array/map) for CFG entities |
| `Structure_descriptor.h` | Entity descriptor: field names, types, and offsets |
| `Structure_expression.h` | Expression evaluation structure |
| `Structure_fileformat.h` | File format identification structure |
| `Structure_hierarchical.h` | Hierarchical (nested) entity structure |
| `Structure_types.h` | Fundamental type definitions for CFG fields |
| `Structure_various.h` | Miscellaneous structure utilities |
| `descriptor_API.c` / `.h` | C API for accessing entity descriptors |

## Related Documentation

- `reader/source/cfgkernel/README.md` — parent directory
- `reader/source/cfgkernel/KERNEL/README.md` — CFG kernel core
