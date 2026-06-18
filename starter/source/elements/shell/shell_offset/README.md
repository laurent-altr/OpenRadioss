# Shell Offset Geometry (`starter/source/elements/shell/shell_offset/`)

Initialises shell mid-surface offset geometry: computes projected node positions and sets up offset connectivity for shells with non-zero offset distances.

## Key Files

| File | Role |
|------|------|
| `shell_offset_ini.F90` | Main offset initialisation: compute offset node positions |
| `shell_offsetp.F90` | Compute offset coordinates using thickness and normal |
| `chk_shell_offset.F90` | Check validity of offset configuration |
| `dim_shell_offsetp.F90` | Compute array dimensions for offset data |
| `inter_offset_itag.F90` | Tag interface nodes affected by offset |
| `same_shellori.F90` | Check consistent shell orientation at offset |
| `sh_offset_jonkt.F90` | Shell offset joint connectivity check |
| `sh_offset_setn.F90` | Set offset normal direction |
| `shell_offset_nproj.F90` | Node projection for offset geometry |

## Description

Shell offset places the element reference surface at a distance from the node plane (e.g., when the node plane is the outer skin and the mid-surface is at half the thickness). `shell_offset_ini.F90` computes the offset node positions and sets up the constraint that ties the offset mid-surface to the actual nodes.

## Related Documentation

- `starter/source/elements/shell/README.md` — parent directory
- `engine/source/modules/interfaces/README.md` — shell offset in contact (sh_offset_mod)
