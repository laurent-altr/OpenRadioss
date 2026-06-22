# Centrifugal and Coriolis Body Force (`engine/source/loads/general/load_centri/`)

Applies centrifugal and Coriolis body forces for rotating reference frames (`/CENTRI`).

## Key Files

| File | Role |
|------|------|
| `cfield.F` | Centrifugal and Coriolis force assembly for explicit solver |
| `cfield_imp.F` | Centrifugal/Coriolis contribution to implicit residual |

## Algorithm

For a node rotating about axis `ω` with position `r`:

```
F_centrifugal = m × ω × (ω × r)          (outward)
F_Coriolis    = −2 m × ω × v_node        (velocity-dependent)
```

`cfield.F` computes both terms and accumulates them into the global force array each step. The rotation axis and angular velocity may be time-varying via a `/FUNCT` curve. `cfield_imp.F` adds the consistent linearisation of both terms to the implicit tangent stiffness matrix.

## Related Documentation

- `engine/source/loads/general/README.md` — parent directory
- `engine/source/loads/README.md` — all load types
