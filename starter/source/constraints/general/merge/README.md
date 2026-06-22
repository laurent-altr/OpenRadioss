# Merge Nodes (`starter/source/constraints/general/merge/`)

Reads /MERGE to merge geometrically coincident nodes that are not topologically connected, creating a common DOF.

## Key Files

| File | Role |
|------|------|
| `hm_preread_merge.F` | Pre-read: count /MERGE blocks, allocate |
| `hm_read_merge.F` | Parse /MERGE: tolerance, node sets |
| `rbmerge_type.F90` | Data type for merged rigid body constraint |

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
