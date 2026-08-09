# Initial Animation Output (`starter/source/output/anim/`)

Writes the t=0 animation frame to the animation file — the initial model geometry and state before the engine starts.

## Key Files

| File | Role |
|------|------|
| `genani1.F` | Generate t=0 animation frame: write geometry, group, and initial field data |
| `initbuf.F` | Initialise animation output buffer |
| `parsor.F` / `parsorc.F` / `parsorf.F` / `parsors.F` | Parse and format animation variable data for different element types |
| `parsor0.F` | Parse animation record header |
| `scanor.F` | Scan and select animation output variables requested by user |
| `ani_fasolfr.F` | Animate fully-integrated solid element frame |
| `ani_segquadfr.F` | Animate quadrilateral surface segment frame |
| `ani_txt.F` | Write text annotations to animation file |
| `aniskew.F` / `aniskewf.F` | Animate skew frame (local reference frame visualisation) |
| `anioff*.F` | Animation file open/close routines (various element types) |
| `dd_ani.F` | Domain decomposition animation: colour elements by MPI rank |
| `tensor0.F` / `tensorc.F` / `tensors.F` | Format stress tensor for animation output |
| `velvec.F` | Format velocity vector for animation |
| `xyznod.F` | Format node coordinates for animation |
| `xyznor.F` | Format element normals for animation |
| `delnumb*.F` | Delete/clear animation buffer entries |
| `dfunc*.F` | Format function output for animation |
| `dmasani*.F` | Format mass distribution for animation |
| `sortho31.F` | Orthonormal basis output (for oriented element visualization) |

## Initial State

The t=0 animation frame contains:
- Node coordinates (undeformed geometry)
- Element connectivity and type
- Group memberships (for colouring by part, material, property)
- Domain decomposition map (MPI rank per element — for debugging parallel runs)
- Initial stress/strain if `/INISTATE` was specified

This frame is written in the same format as subsequent animation frames, so the engine can simply append to the same file.

## Related Documentation

- `starter/source/output/README.md` — parent output directory
- `engine/source/output/README.md` — subsequent animation frames written by engine
