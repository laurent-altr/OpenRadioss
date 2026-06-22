# Cluster Output (`engine/source/output/cluster/`)

Writes cluster (element-group) TH output: aggregated results over user-defined element sets.

## Key Files

| File | Role |
|------|------|
| `clusterf.F` | Compute cluster (element group) resultant forces and moments |
| `read_cluster.F` | Read cluster output configuration from restart |
| `w_cluster.F` | Write cluster results to TH output file |

## Description

A cluster is a named group of elements for which averaged or summed output quantities (force resultant, energy, centroid position) are written to the TH file each output step. `clusterf.F` computes the resultant by summing over all elements in the cluster. Used for monitoring load paths through structural components (B-pillar resultant force, door intrusion energy) without full-field post-processing.

## Related Documentation

- `engine/source/output/README.md` — parent output directory
- `engine/source/output/th/README.md` — TH output (writes the cluster data)
