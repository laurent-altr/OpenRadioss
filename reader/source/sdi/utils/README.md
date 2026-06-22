# SDI Utils (`reader/source/sdi/utils/`)

Foundational value types and helpers used throughout the SDI library for
storing, querying, and passing entity attribute values.

## Key Files

| File | Role |
|------|------|
| `sdiValue.h` | `sdiValue` — tagged union holding `int`, `double`, `string`, `bool`, or array; `sdiIdentifier` — typed attribute key |
| `sdiValueEntity.h` | `sdiValueEntity` (reference to another entity by type+id) + `sdiValueEntityType` (entity-type value) |
| `sdiValuePtr.h` | `sdiValuePtr` — smart pointer wrapper for heap-allocated `sdiValue`s |
| `sdiTriple.h` | `sdiTriple<T>` — three-component vector (coordinates, direction vectors) |
| `sdiUtilsDefs.h` | Library-wide `SDIUTILS_DECLS` export macros and type aliases |

## Related Documentation

- `reader/source/sdi/utils/_private/README.md` — implementation files
- `reader/source/sdi/interface/README.md` — entity API that consumes these types
- `reader/source/sdi/README.md` — parent SDI library
