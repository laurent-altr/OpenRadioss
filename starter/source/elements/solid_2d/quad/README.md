# 2D Quad Element Initialisation (`starter/source/elements/solid_2d/quad/`)

Starter initialisation for 4-node quadrilateral 2D solid elements with one-point (reduced) integration.

## Key Files

| File | Role |
|------|------|
| `qinit2.F` | Main 2D quad element initialisation |
| `qmasi2.F` | Compute 2D quad nodal mass |
| `qmasi2b.F` | Alternate mass computation for hourglass control |
| `qcoor2.F` | Compute 2D quad local coordinate system |
| `qdlen2.F` | Compute 2D quad characteristic length |
| `qmorth2.F` | Set orthotropic material axes for 2D quad |
| `qrcoor2.F` | Compute reference coordinate system |
| `qvoli2.F` | Compute 2D quad element volume (area) |
| `qgrhead.F` | Write 2D quad group header to restart |
| `qgrtails.F` | Write 2D quad group tail data to restart |
| `reordr.F` | Reorder node numbering for consistent orientation |

## Related Documentation

- `starter/source/elements/solid_2d/README.md` — parent directory
