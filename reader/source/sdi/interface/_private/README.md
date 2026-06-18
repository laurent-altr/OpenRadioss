# SDI Interface — Private Implementation (`reader/source/sdi/interface/_private/`)

Compiled bodies and internal data classes for the SDI interface layer.

## Key Files

| File | Role |
|------|------|
| `sdiEntity.cxx` / `sdiEntityBase.cxx` | `EntityRead` / `EntityEdit` / `EntityBase*` method bodies |
| `sdiEntityData.h` / `sdiEntityData.cxx` | Internal per-entity data storage struct |
| `sdiElement.cxx` | `ElementRead` / `ElementEdit` method bodies |
| `sdiElementData.h` | Internal per-element data |
| `sdiNode.cxx` | `NodeRead` / `NodeEdit` method bodies |
| `sdiNodeData.h` | Internal per-node data |
| `sdiHandles.cxx` | Handle implementation |
| `sdiFilter.cxx` | Filter predicate evaluation |
| `sdiSelectionBase.cxx` / `sdiSelectionData.h` | Selection iterator internals |
| `sdiModelViewPrivate.h` / `sdiModelViewPrivate.cxx` | Private `ModelView` state (entity DB, index maps) |

## Related Documentation

- `reader/source/sdi/interface/README.md` — public headers (parent directory)
