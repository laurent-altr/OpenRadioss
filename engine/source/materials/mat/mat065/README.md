# LAW65 — Laminated Composite (Layer Stack) (`engine/source/materials/mat/mat065/`)

Multi-layer composite shell material: per-ply orthotropic elastic-plastic
response with ply-by-ply failure tracking in a layered stack.

## Key Files

| File | Role |
|------|------|
| `sigeps65.F` | Main stress update: ply-by-ply integration |
| `sigeps65c.F` | Shell variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
