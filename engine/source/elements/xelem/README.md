# X-Element (Extra Element) Force Computation (`engine/source/elements/xelem/`)

Computes forces for extra elements (X-elements) — special-purpose connector elements not falling into standard categories.

## Key Files

| File | Role |
|------|------|
| `xgrhead.F` / `xgrtails.F` | Write X-element group header/tail to restart |
| `xinit3.F` | Initialise X-element at engine startup |
| `xini28.F` | Initialise X-element type 28 |
| `xini29.F` | Initialise X-element type 29 |
| `xini30.F` | Initialise X-element type 30 |
| `xini31.F` | Initialise X-element type 31 |

## X-Elements

X-elements are specialised connector elements for specific applications:
- Spot weld simulation (point connector with failure)
- Adhesive bond (area-distributed stiffness)
- Special kinematic constraints not handled by standard elements

Each type (28–31) has its own force law and failure criterion, but all follow the same API: two or more connected nodes, stiffness in multiple DOF, optional failure when a force/displacement limit is reached.

## Related Documentation

- `engine/source/elements/README.md` — parent elements overview
- `starter/source/elements/xelem/README.md` — X-element initialisation
