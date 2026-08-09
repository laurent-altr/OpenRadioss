# Cluster Output (`starter/source/output/cluster/`)

Reads cluster definitions — groups of elements or nodes clustered for aggregate output.

## Key Files

| File | Role |
|------|------|
| `hm_read_cluster.F` | HM binary reader for `/CLUSTER` definitions |
| `itrimhpsort.F` | Trim and sort cluster member lists (remove duplicates, order by ID) |

## Clusters

A cluster (`/CLUSTER`) aggregates multiple elements or nodes and outputs a single time-history result representing the cluster (sum, average, or max). Used for:
- Computing total force over a bolt pattern (sum of individual bolt forces)
- Average stress over a critical region
- Mass-weighted average velocity of a component

## Related Documentation

- `starter/source/output/README.md` — parent output directory
