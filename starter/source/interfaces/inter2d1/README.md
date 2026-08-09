# 2D Interface Geometry (`starter/source/interfaces/inter2d1/`)

2D contact geometry initialisation: builds segment lists, checks orientation, and computes initial gaps for 2D (plane strain/axisymmetric) contact interfaces.

## Key Files

| File | Role |
|------|------|
| `i1chk2.F` | Check 2D slave node proximity to master segments |
| `i1tid2.F` | Assign slave nodes to master segments (2D nearest segment) |
| `i3pen2.F` | Compute initial penetration for TYPE3 2D |
| `i3sti2.F` | Compute initial stiffness for TYPE3 2D |
| `inare2.F` | Compute 2D segment areas (lengths) |
| `inint2.F` | Main 2D interface initialisation |
| `inori2.F` | Orient 2D master surface segments (consistent outward normal) |
| `inrch2.F` | 2D search radius computation |
| `invoi2.F` | 2D neighbour (voisin) list for master segments |

## Description

The `inter2d1` routines mirror the 3D contact initialisation (`inter3d1`) for 2D analysis domains. `inint2.F` orchestrates the setup: orient segments (`inori2.F`), compute areas, build the neighbour list (`invoi2.F`), and assign each slave node to its master segment (`i1tid2.F`).

## Related Documentation

- `starter/source/interfaces/inter3d1/README.md` — 3D contact geometry initialisation
- `starter/source/interfaces/interf1/README.md` — top-level contact setup
