# TYPE21 Interface (`engine/source/interfaces/int21/`)

TYPE21 — Node-to-segment contact between ALE mesh and Lagrangian structure.

## Key Files

| File | Role |
|------|------|
| `i21ass3.F` | Assemble TYPE21 forces |
| `i21cor3.F` | Kinematic correction |
| `i21cor3t.F` | Kinematic correction with time integration |

## TYPE21 (ALE-Lagrangian)

TYPE21 handles the coupling between an ALE fluid mesh and a Lagrangian structural mesh at the fluid-structure interface. The Lagrangian nodes are treated as slave nodes against the ALE segment master surface.

## Related Documentation

- `engine/source/interfaces/README.md` — parent directory
- `engine/source/ale/inter/README.md` — ALE interface coupling
