# Starter Fluid Pressure Load (`starter/source/loads/general/pfluid/`)

Reads `/PFLUID` fluid-pressure-on-structure load definition.

## Key Files

| File | Role |
|------|------|
| `hm_read_pfluid.F` | Read `/PFLUID` card: FSI coupling surface, ALE material reference |
| `hm_preread_pfluid.F` | Pre-read pass for array sizing |

## Related Documentation

- `starter/source/loads/general/README.md` — parent directory
- `engine/source/loads/general/pfluid/README.md` — engine FSI pressure transfer
