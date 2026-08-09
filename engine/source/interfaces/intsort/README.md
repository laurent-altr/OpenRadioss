# Contact Sorting (Broad Phase) (`engine/source/interfaces/intsort/`)

Implements the contact broad-phase search — identifying which node-segment pairs to check in the expensive narrow-phase penetration test.

## Key Files

| File | Role |
|------|------|
| `check_sorting_criteria.F90` | Validate that sorting results meet minimum coverage criteria |
| `collision_mod.F` | Collision detection module: voxel-based spatial hashing |
| `compare_cand.cpp` | C++ candidate comparison for sorting (de-duplication) |
| `fill_voxel.F90` | Fill voxel grid with segment bounding boxes |
| `i10buce.F` | TYPE10 bucket cell fill |
| `i10main_tri.F` | Main sorting for triangular segments |
| `i10opt_opt_tri.F` | Optimised triangular sort |
| `i10optcd.F` | Optimised cell detection |
| `i10sto.F` | Store detected contact pairs |
| `i10trc.F` | TYPE10 contact trace |

## Voxel-Based Broad Phase

The broad-phase search divides the model's bounding box into voxels (grid cells). Each contact segment is placed in the voxels it touches. A slave node is then only tested against segments in its voxel and the 26 adjacent voxels:

```
fill_voxel:   for each segment, find voxels it overlaps
for each slave node:
  find node's voxel
  for each segment in adjacent voxels:
    add (node, segment) to candidate list
```

This reduces O(N²) brute-force to O(N) average with appropriate voxel sizing.

## Related Documentation

- `engine/source/interfaces/README.md` — parent interfaces directory
- `engine/source/interfaces/int07/README.md` — TYPE7 uses this sorting
- `engine/source/interfaces/generic/README.md` — generic sorting utilities
