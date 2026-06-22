# Ply Stack Definition (`starter/source/properties/composite_options/stack/`)

Reads `/STACK` laminate definitions: ordered sequences of plies with material law, thickness, and fiber angle for each lamina.

## Key Files

| File | Role |
|------|------|
| `lecstack_ply.F` | Read `/STACK` card: ply sequence, material IDs, angles, thicknesses |
| `preplyxfem.F` | Pre-process ply data for XFEM intralaminar crack insertion |

## Algorithm

`lecstack_ply.F` reads:
1. Number of plies `N_ply`
2. For each ply: material law ID, ply thickness, fiber angle, integration point count
3. Optional symmetry flag (half-stack mirrored)

This data is stored in the ply parameter arrays (`ply_param_mod`) and linked to the shell element property. At engine startup the ply data drives the through-thickness Gauss integration, with each integration point seeing the local fiber orientation of its ply.

`preplyxfem.F` pre-computes the ply interfaces (interlaminar planes) needed to insert XFEM delamination crack surfaces between plies.

## Related Documentation

- `starter/source/properties/composite_options/README.md` — parent composite options
- `starter/source/properties/composite_options/drape/README.md` — drape-mapped orientations
- `common_source/modules/mat_elem/README.md` — ply_param_mod data structure
