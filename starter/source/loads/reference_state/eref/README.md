# Starter EREF (Element Reference State) (`starter/source/loads/reference_state/eref/`)

Reads `/EREF` element reference state definitions: initial stress and strain tensors applied to elements at t = 0.

## Key Files

| File | Role |
|------|------|
| `hm_read_eref.F` | Read `/EREF` card: element set, per-component initial stress/strain values |

## Related Documentation

- `starter/source/loads/reference_state/README.md` — parent reference state directory
- `starter/source/loads/general/preload/README.md` — bolt preload (scalar preload)
- `starter/source/elements/initia/README.md` — initial state loading in starter
