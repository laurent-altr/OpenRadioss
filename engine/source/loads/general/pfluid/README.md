# Fluid Pressure on Structure (`engine/source/loads/general/pfluid/`)

Transfers pressure from an ALE/Euler fluid mesh onto the wetted surface of a Lagrangian structural mesh.

## Key Files

| File | Role |
|------|------|
| `pfluid.F` | Map ALE cell pressures to structural segment nodal forces |

## Algorithm

`pfluid.F` is called after the ALE solver updates cell pressures. For each structural segment that lies on the fluid-structure interface:

1. Locate the overlapping ALE cell(s) via the TYPE22/TYPE21 interface mapping
2. Interpolate cell pressure to segment centroid
3. Convert to nodal forces: `F_node += P_fluid × A_seg × n_hat / n_nodes`

This implements the fluid-pressure boundary condition side of the ALE–Lagrangian FSI coupling. The structural forces feed into the next explicit step, while the structural velocity boundary condition feeds back to the ALE mesh.

## Related Documentation

- `engine/source/loads/general/README.md` — parent directory
- `engine/source/interfaces/int22/README.md` — immersed boundary ALE contact
- `engine/source/ale/inter/README.md` — ALE–Lagrangian coupling
