# CFG–SDI Bridge (`reader/source/cfgkernel/sdi/`)

Adapter headers that bridge the CFG kernel to the SDI (Solver Data Interface) abstraction layer, allowing the CFG model view to be exposed as an SDI model.

## Key Files

| File | Role |
|------|------|
| `sdiCFGTypeMapper.h` | Maps CFG entity type IDs to SDI entity types |
| `sdiModelViewCFG.h` | SDI model view backed by a CFG kernel instance |
| `sdiModelViewPO.h` | SDI model view backed by a pre-object (PO) kernel |
| `sdiModelViewPOLSDyna.h` | SDI model view for LS-DYNA pre-object format |

## Related Documentation

- `reader/source/cfgkernel/README.md` — parent directory
- `reader/source/sdi/README.md` — SDI abstract interface
