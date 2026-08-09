# Starter Pressure Blast Load (`starter/source/loads/pblast/`)

Reads `/LOAD/PBLAST` blast pressure load definition.

## Key Files

| File | Role |
|------|------|
| `hm_read_pblast.F` | Read `/LOAD/PBLAST` card: charge weight, detonation point, blast model type, segment set |
| `hm_preread_pblast.F` | Pre-read pass for array sizing |

## Related Documentation

- `starter/source/loads/README.md` — parent loads directory
- `engine/source/loads/pblast/README.md` — engine blast pressure application
