# Starter Cylindrical Pressure (`starter/source/loads/general/load_pcyl/`)

Reads `/LOAD/PCYL` cylindrical pressure input and handles domain decomposition of the cylindrical pressure region.

## Key Files

| File | Role |
|------|------|
| `hm_read_pcyl.F` | Read `/LOAD/PCYL` card: axis, radius, pressure magnitude, function |
| `domain_decomposition_pcyl.F` | Split cylindrical pressure region across MPI domains |
| `split_pcyl.F` | Partition cylinder segment set across domains |
| `write_pcyl.F` | Write cylindrical pressure data to restart |

## Related Documentation

- `starter/source/loads/general/README.md` — parent directory
- `engine/source/loads/general/load_pcyl/README.md` — engine application
