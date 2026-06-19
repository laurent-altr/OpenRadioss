# Starter LAW104 — Elastic-plastic with NICE / CPPM return-mapping and damage (`starter/source/materials/mat/mat104/`)

Reads `/MAT/LAW104` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `cnloc_mat104_ini.F` | `CNLOC_MAT104_INI` — starter helper routine |
| `hm_read_mat104.F` | `HM_READ_MAT104` — reads `/MAT/LAW104` keyword parameters into PM/IPM |
| `law104_upd.F` | `LAW104_UPD` — post-read update: derived quantities, defaults, consistency checks |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat104/README.md` — corresponding engine law
