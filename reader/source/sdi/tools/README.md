# SDI Tools (`reader/source/sdi/tools/`)

Utility classes supporting the SDI library infrastructure: ID management,
type mapping, message dispatch, and selection observation.

## Key Files

| File | Role |
|------|------|
| `sdiIdManager.h` | `SDIIdManager` — allocates and recycles unique integer IDs for SDI entities |
| `sdiTypeMapper.h` | `SDITypeMapper` / `SDITypeCorrespondence` — maps keyword strings to `EntityType` enums; supports multi-format registries |
| `sdiMessageHandler.h` | Message/warning callback interface |
| `sdiSelectionObserver.h` | Observer interface for selection-change notifications |

## Related Documentation

- `reader/source/sdi/interface/README.md` — entity types and `EntityType` enum
- `reader/source/sdi/README.md` — parent SDI library
