# Drape Mapping (`starter/source/properties/composite_options/drape/`)

Reads and applies drape-mapped fiber orientations for woven composite shells: imports fiber angle fields from draping simulation results and maps them onto the FE mesh.

## Key Files

| File | Role |
|------|------|
| `hm_read_drape.F` | Read `/DRAPE` card: fiber angle field from draping simulation |
| `shellthk_upd.F` | Update shell thickness from drape-mapped ply thickness field |

## Algorithm

Composite preform draping introduces location-varying fiber angles as flat fabric conforms to double-curved surfaces. `hm_read_drape.F` reads a per-element or per-node fiber angle field (typically exported from a draping tool such as HyperFiber) and stores it in the ply orientation array. `shellthk_upd.F` updates the element thickness from the drape-induced thinning (fabric stretches on convex surfaces, thickens on concave).

The drape data overrides the nominal ply angle from `/STACK` on an element-by-element basis, enabling high-fidelity composite crash simulation with manufacturing-process-induced fiber waviness and angle variation.

## Related Documentation

- `starter/source/properties/composite_options/README.md` — parent composite options
- `starter/source/properties/composite_options/stack/README.md` — ply stack definition
