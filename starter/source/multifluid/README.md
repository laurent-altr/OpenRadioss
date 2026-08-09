# Starter Multi-Fluid Subsystem

This subsystem reads and initialises multi-fluid (multi-material ALE) definitions.

## Key Files

| File | Role |
|------|------|
| `multi_connectivity.F` | Build multi-fluid cell connectivity |
| `multi_check_eos.F` | Validate that each fluid species has a valid EOS |
| `multi_check_psh.F` | Validate pressure shell elements at fluid boundaries |
| `multi_unplug_neighbors.F` | Identify and disconnect isolated cell clusters |
| `multifluid_global_tdet.F` | Compute global thermal determinant for multi-fluid |

## Initialisation

The starter:
1. Reads `/MULTIFLUID` and per-fluid `/MAT` + `/EOS` definitions
2. Validates that each material used in an ALE cell has an EOS (required for multi-fluid)
3. Builds the cell connectivity for the multi-fluid domain
4. Assigns initial volume fractions from `/INIVOL` (handled in `initial_conditions/`)
5. Checks for isolated cell clusters that would cause pressure solve failures

## Related Documentation

- `engine/source/multifluid/README.md` — runtime multi-fluid evolution
- `starter/source/ale/README.md` — ALE domain setup (multi-fluid uses the ALE mesh)
- `common_source/eos/README.md` — EOS models validated here
