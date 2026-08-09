# Truss Element Force Computation (`engine/source/elements/truss/`)

Computes internal forces for truss elements (2-node axial-force-only members).

## Key Files

| File | Role |
|------|------|
| `dt1lawt.F` | Compute stable time step for truss elements: `dt = L / c` |
| `tcoori.F` | Compute truss local coordinate (unit axial vector) |
| `tgrtails.F` / `tgrhead.F` | Write truss group header/tail to restart (restart I/O) |
| `tibuf3.F` | Truss element buffer initialisation |
| `tinit3.F` | Truss element initialisation |
| `tmass.F` | Compute truss nodal mass |
| `tsigini.F` | Apply initial stress to truss elements |

## Truss Element

A truss element (`/TRUSS`) carries only axial force — no bending, shear, or torsion. The force computation is:
1. Compute current length `L` and unit axial vector `e`
2. Compute axial strain `ε = (L - L₀) / L₀`
3. Evaluate material law → axial stress `σ`
4. Internal force: `F = σ A e` (scatter to both nodes)

The simplicity makes truss elements very cheap. They are used for cables, tendons, rebar in concrete (embedded), and structural frameworks.

## Related Documentation

- `engine/source/elements/README.md` — parent elements overview
- `starter/source/elements/README.md` — truss initialisation (same directory as beam)
