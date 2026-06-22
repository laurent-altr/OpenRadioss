# Starter XREF (Cross-Reference State) (`starter/source/loads/reference_state/xref/`)

Reads `/XREF` cross-reference state: maps element material points from one mesh to another for state transfer between analyses.

## Key Files

| File | Role |
|------|------|
| `hm_read_xref.F` | Read `/XREF` card: source mesh reference, mapping parameters |

## Description

`/XREF` performs a spatial interpolation of stress/strain state from a source mesh (e.g., from a forming simulation) onto the target crash mesh. The starter reads the source DYNAIN state file, locates source elements that contain each target integration point, and interpolates the initial state tensor field. This enables forming-to-crash transfer with continuous material history, more accurate than simple preload specification.

## Related Documentation

- `starter/source/loads/reference_state/README.md` — parent directory
- `engine/source/output/dynain/README.md` — DYNAIN state file used as source
