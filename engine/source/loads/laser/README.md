# Laser Load (`engine/source/loads/laser/`)

Applies a spatially and temporally varying heat flux from a laser beam to the structural surface (`/LOAD/LASER`).

## Key Files

| File | Role |
|------|------|
| `laser1.F` | Laser beam model 1: Gaussian intensity profile on a flat surface |
| `laser2.F` | Laser beam model 2: moving laser spot with scan-path definition |

## Algorithm

The laser delivers a surface heat flux `q(x, t)` with a Gaussian spatial distribution:

```
q(r, t) = P(t) / (π r0²) × exp(−r² / r0²)
```

where `r` is distance from the beam centre, `r0` is the spot radius, and `P(t)` is the power time history. The flux is accumulated onto nodal thermal loads:

```fortran
Q_node += q(r_node, t) * A_node
```

`laser2.F` adds a scan-path capability: the beam centre moves along a user-defined trajectory `/FUNCT` in the surface plane, enabling simulation of laser welding and additive manufacturing processes.

## Related Documentation

- `engine/source/loads/README.md` — parent loads directory
- `engine/source/loads/general/README.md` — mechanical loads
