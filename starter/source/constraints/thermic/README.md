# Thermal Constraints (`starter/source/constraints/thermic/`)

Reads thermal boundary conditions: imposed temperature and heat flux on node groups.

## Key Files

| File | Role |
|------|------|
| `glob_therm_init.F90` | Initialise global thermal constraint data structures |
| `hm_preread_impflux.F` | Pre-read: count imposed heat flux definitions |
| `hm_preread_imptemp.F` | Pre-read: count imposed temperature definitions |
| `hm_read_impflux.F` | Parse /IMPFLUX: heat flux boundary condition |
| `hm_read_imptemp.F` | Parse /IMPTEMP: imposed temperature boundary condition |

## Related Documentation

- `starter/source/constraints/README.md` — parent directory
- `engine/source/constraints/thermic/README.md` — engine-side thermal enforcement
