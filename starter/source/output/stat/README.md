# Model Statistics (`starter/source/output/stat/`)

Reads model statistics output settings.

## Key Files

| File | Role |
|------|------|
| `hm_read_stat.F` | HM binary reader for model statistics output configuration |

## Model Statistics

The statistics output (`/STAT`) controls whether the starter prints a detailed model summary to the `.out` file, including:
- Element count by type, property, material
- Node count and coordinate bounds
- Mass distribution by part
- Timestep statistics (min/max element dt, controlling element)

This is enabled by default and can be controlled via `/STAT`.

## Related Documentation

- `starter/source/output/README.md` — parent output directory
