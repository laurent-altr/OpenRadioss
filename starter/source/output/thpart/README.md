# Part Time History (`starter/source/output/thpart/`)

Reads `/TH/PART` definitions — time history output aggregated at the part level.

## Key Files

| File | Role |
|------|------|
| `hm_read_thpart.F` | HM binary reader for `/TH/PART` definitions |

## Part-Level Output

`/TH/PART` records aggregate quantities for all elements in a part:
- Total part mass
- Total part internal energy
- Part kinetic energy
- Centre-of-gravity position and velocity
- Total resultant force on part

This is cheaper than element-level output and gives a useful summary of energy distribution between parts during a crash/impact simulation.

## Related Documentation

- `starter/source/output/th/README.md` — general TH output setup
- `starter/source/output/README.md` — parent output directory
