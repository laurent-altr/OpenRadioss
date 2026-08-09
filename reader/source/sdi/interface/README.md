# SDI Interface (`reader/source/sdi/interface/`)

Public C++ API for the Solver Data Interface (SDI) in-memory model database.
Provides read/write views over entities (nodes, elements, materials, etc.),
selection iterators, and entity handles.

## Key Files

| File | Role |
|------|------|
| `sdiModelView.h` | `ModelViewRead` / `ModelViewEdit` — top-level model database handles; all entity access goes through these |
| `sdiEntity.h` | `EntityRead` / `EntityEdit` — read-only and read-write views over a single named entity |
| `sdiElement.h` | `ElementRead` / `ElementEdit` — element-specific views (connectivity, part reference) |
| `sdiEntityBase.h` | `EntityBaseRead` / `EntityBaseEdit` — common base for all entity types |
| `sdiHandles.h` | `HandleRead` / `HandleEdit` / `HandleElementEdit` — lightweight opaque entity references |
| `sdiFilter.h` | Predicate-based entity filter for selections |
| `sdiSelection.h` | Selection iterator over a filtered entity set |
| `sdiNode.h` | `NodeRead` / `NodeEdit` — node-specific views (coordinates, frame) |
| `sdiDefs.h` | Library-wide constants, `EntityType` enum, `Status` enum |

## Design

SDI separates *handles* (cheap, copyable) from *views* (`Entity*`, `Element*`);
a view wraps a handle and provides typed attribute access.  `ModelViewEdit`
extends `ModelViewRead` with `CreateEntity`, `DeleteEntity`, and `SetAttribute`
operations.  All attribute access is string-keyed to remain independent of
solver-specific data layouts.

## Related Documentation

- `reader/source/sdi/interface/_private/README.md` — implementation files
- `reader/source/sdi/utils/README.md` — `sdiValue` / `sdiIdentifier` types
- `reader/source/sdi/tools/README.md` — `SDIIdManager`, `SDITypeMapper`
- `reader/source/sdi/README.md` — parent SDI library
