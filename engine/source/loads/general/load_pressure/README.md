# Surface Pressure Load (`engine/source/loads/general/load_pressure/`)

Applies uniform scalar pressure normal to a segment surface (`/LOAD/PRES`).

## Key Files

| File | Role |
|------|------|
| `load_pressure.F` | Compute area-weighted nodal forces from pressure on a segment set |
| `press_seg3.F` | Pressure on triangular (3-node) segments |

## Algorithm

For each segment in the load set, pressure × segment area is distributed to the segment nodes:

```fortran
F_node_i += P(t) * A_seg * n_hat / n_nodes_on_seg
```

`press_seg3.F` handles the triangular-segment special case (used for shell meshes with mixed quads and triangles). Pressure magnitude is scaled by a `/FUNCT` time curve each step. Sign convention: positive pressure acts inward (compression).

## Related Documentation

- `engine/source/loads/general/README.md` — parent directory
- `engine/source/loads/general/load_pcyl/README.md` — cylindrical pressure variant
