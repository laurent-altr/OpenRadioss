# Starter ALE Material (`starter/source/materials/ale/`)

Reads ALE and Euler material definitions: initial thermodynamic state for fluid cells.

## Key Files

| File | Role |
|------|------|
| `ale_euler_init.F` | Initialise ALE/Euler cell material state from input |
| `read_ale_mat.F` | Read ALE material card: density, pressure, velocity fields |
| `read_euler_mat.F` | Read Euler material card: multi-material region initialisation |

## Description

ALE materials define the initial state (density, pressure, velocity, temperature) for fluid regions in the ALE mesh. `ale_euler_init.F` fills the ALE cell state arrays from the material and geometry data. `read_euler_mat.F` handles the multi-material initialisation where different ALE materials occupy different geometric zones defined by sets or analytic shapes.

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `engine/source/ale/README.md` — ALE solver in engine
