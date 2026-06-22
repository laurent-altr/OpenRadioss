# Cylindrical Pressure Load (`engine/source/loads/general/load_pcyl/`)

Applies pressure on a cylindrical surface defined by axis, radius, and segment set (`/LOAD/PCYL`).

## Key Files

| File | Role |
|------|------|
| `pressure_cyl.F` | Compute and apply nodal forces from cylindrical pressure distribution |

## Algorithm

`pressure_cyl.F` iterates over segments in the user-defined segment set, tests whether each segment is within the cylindrical region, and projects the pressure magnitude onto segment face normals:

```fortran
F_node += pressure * area_fraction * face_normal
```

Useful for modelling internal pressure in cylindrical tubes, gun barrels, or pressure vessels where the geometry is defined analytically rather than by a mesh surface.

## Related Documentation

- `engine/source/loads/general/README.md` — parent directory
- `engine/source/loads/general/load_pressure/README.md` — flat-surface pressure
