# Engine Seatbelt Tools (`engine/source/tools/seatbelts/`)

Applies seatbelt cable/retractor kinematics each time step: computes belt forces and constrains belt-node motion.

## Key Files

| File | Role |
|------|------|
| `compute_contact_force_guide.F90` | Compute contact force at guide/slip-ring contact |
| `guided_cable_force.F90` | Guided cable force: cable through a frictionless slot |
| `kine_seatbelt_force.F` | Seatbelt segment force from cable tension and friction |
| `kine_seatbelt_vel.F` | Seatbelt node velocity update from retractor kinematics |
| `material_flow.F` | Belt material flow through slip rings (pay-in/pay-out) |

## Algorithm

Seatbelts are modelled as 1D cable elements with:
1. Tension-only force `T = k × δ` (no compression) from `kine_seatbelt_force.F`
2. Friction at slip rings/guides using Capstan equation: `T_out = T_in × e^(μθ)` from `guided_cable_force.F90`
3. Retractor pay-in/pay-out from `material_flow.F` (belt length changes as retractor locks/unlocks)

## Related Documentation

- `engine/source/tools/README.md` — parent tools directory
- `starter/source/tools/seatbelts/README.md` — seatbelt definition
