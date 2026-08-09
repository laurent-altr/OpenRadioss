# Group Assembly (`starter/source/model/group/`)

Assembles and resolves group definitions into entity lists after all sets and clauses have been evaluated.

## Key Files

| File | Role |
|------|------|
| `group_ini.F` | Main group initialisation: evaluate all group definitions, populate entity lists |
| `gr_entity_ini.F` | Initialise entity-level group membership |
| `c_gr_entity.F` | C-linkage wrapper for group entity operations |
| `islin_ini.F` | Initialise line groups (`GRLINE`, `GRSEG`) |
| `isurf_ini.F` | Initialise surface groups (`GRSURF`) |
| `line_decomp.F` | Decompose a line group into individual segment lists |
| `subset_ini.F` | Initialise subsets (named collections of parts for output and contact) |

## Group Resolution Order

Groups can reference other groups (e.g., `GRNOD` = union of two other `GRNOD`s). The resolution must follow dependency order:

```
1. Resolve primitive groups (explicit node/element lists)
2. Evaluate set clauses (box, surface, plane selections)
3. Apply boolean operations (union, intersection)
4. Build line and surface groups from element faces
5. Resolve subset definitions (named part collections)
```

`group_ini.F` orchestrates this sequence, calling into `starter/source/model/sets/` for clause evaluation.

## Related Documentation

- `starter/source/model/README.md` — parent model directory
- `starter/source/model/sets/README.md` — set/clause evaluation
- `starter/source/groups/README.md` — group types table and validation
