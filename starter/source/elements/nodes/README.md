# Node Utilities (`starter/source/elements/nodes/`)

Node geometry initialization and mesh connectivity utilities used during starter model assembly.

## Key Files

| File | Role |
|------|------|
| `merge_node.F` | Merge coincident nodes within tolerance — the core auto-merge routine |
| `merge_bucket_search.F` | Bucket (spatial hash) search to find candidate node pairs for merging |
| `auto_node_merge.F` | Driver: iterates merge candidates, applies `merge_node.F`, updates connectivity |
| `reconnect.F` | Reconnect element connectivity after node merging (update element tables) |
| `constit.F` | Compute constitutive (material) frame at each node for anisotropic elements |

## Node Merging

Node merging (`/MERGE/NODE`) is essential for assembling meshes from multiple parts where coincident nodes must become a single node:

```
auto_node_merge
  └── merge_bucket_search  — build spatial buckets, find pairs within tolerance
        └── merge_node     — merge each pair: keep lower ID, remap higher ID
              └── reconnect — update element connectivity tables
```

The tolerance is specified in the input deck (`/MERGE/NODE, tol`). Nodes within distance `tol` are merged.

## Bucket Search

`merge_bucket_search.F` divides the bounding box into a grid of buckets. Each node is hashed to its bucket and checked against all nodes in the same and neighbouring buckets. This gives O(n) average complexity vs O(n²) for a brute-force scan.

## Related Documentation

- `starter/source/elements/README.md` — parent directory; covers all element types
- `starter/source/model/submodel/README.md` — submodel boundary node merging
