# Element Buffer Initialisation (`starter/source/elements/elbuf_init/`)

Allocates and fills the element buffer (`ELBUF`) during starter model setup, then writes it to the `_0001.rad` restart file.

## Key Files

| File | Role |
|------|------|
| `elbuf_ini.F` | Main ELBUF initialisation: set structure pointers, allocate arrays |
| `allocbuf_auto.F` | Allocate buffer arrays based on element group metadata |
| `deallocate_buffer.F` | Free ELBUF arrays after writing to restart |
| `deallocate_one_element_group.F` | Free one group's buffer slice |
| `ini_eos_vars.F` | Initialise EOS state variables (density, internal energy) from material parameters |
| `ini_mlaw_vars.F` | Initialise material law history variables (plastic strain, damage, etc.) |
| `ini_prop_vars.F` | Initialise property-dependent variables (thickness, integration weights) |
| `ini_outmax_auto.F` | Allocate output-max arrays (`/ANIM/SHELL/EPSP`, `/H3D/SOLID/STRESS`, etc.) |
| `initvars_auto.F` | Zero all element state arrays before initial fill |
| `zerovars_auto.F` | Zero selected variable groups |
| `init_mlaw_tag.F` | Set material law tags for element-level fast dispatch |
| `r2r_matparam_copy.F` | Copy material parameters from global material tables into per-element ELBUF slots |
| `suderi3.F` / `suinit3.F` | Read solid element connectivity and initialise geometry fields |
| `ush_init.F90` | Compute initial element metric tensors (Jacobian, thickness normals) |

## Design

ELBUF is a single large flat array per element group, laid out as:

```
[elem 1 geometric fields] [elem 1 material state] [elem 2 geometric fields] [elem 2 material state] ...
```

This layout maximises cache locality in the engine's inner force loops, where each element's data is accessed sequentially.

## Related Documentation

- `engine/source/elements/elbuf/README.md` — engine-side ELBUF management
- `starter/source/elements/README.md` — parent elements directory
