# LAW32 — Brittle Elastic (`engine/source/materials/mat/mat032/`)

Linear elastic material with brittle fracture: element deletion once a
maximum principal stress or strain threshold is reached.

## Key Files

| File | Role |
|------|------|
| `m32elas.F` | Elastic stress update |
| `m32plas.F` | Brittle failure check and element deletion flag |
| `sigeps32c.F` | Shell stress-update wrapper |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
