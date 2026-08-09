# ALE Interface (`engine/source/ale/inter/`)

Handles the fluid-structure and fluid-fluid interfaces in ALE simulations — coupling between the ALE/Euler mesh and Lagrangian structural elements.

## Key Files

| File | Role |
|------|------|
| `intal1.F` | TYPE1 ALE interface: Lagrangian nodes embedded in ALE mesh — interpolate fluid state to Lagrangian |
| `intal2.F` | TYPE2 ALE interface: coupling force exchange between fluid and structure |
| `intal3.F` | TYPE3 ALE interface |
| `intal4.F` | TYPE4 ALE interface |
| `iqela1.F` | Interface element quality for TYPE1 |
| `iqela2.F` | Interface element quality for TYPE2 |
| `iqela3.F` | Interface element quality for TYPE3 |
| `bcs3v.F` | Apply velocity BCs at ALE interface |
| `int12w.F` | TYPE12 ALE interface weight computation |
| `shapeh.F` | Shape functions for ALE interface interpolation |

## ALE Interface Coupling

When a Lagrangian structure (e.g., a structure hit by a blast wave) is embedded in an ALE fluid mesh:
1. The fluid mesh moves through the Lagrangian nodes (ALE mesh velocity ≠ material velocity)
2. At the interface, fluid pressure is transferred to the structure as force
3. The structure's velocity provides a moving boundary condition for the fluid
4. The Sutherland-Hodgman clipper (`common_source/tools/clipping/`) computes intersection areas

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
- `common_source/tools/clipping/README.md` — polygon clipping for ALE interface areas
