# fail/

## Purpose
Failure criteria helpers shared between starter and engine. Currently contains the Newman-Raju stress intensity factor formula used by windshield/glass fracture models.

## Files

| File | Description |
|------|-------------|
| `newman_raju.F90` | Module `newman_raju_mod` — subroutine `newman_raju` computes the geometry correction factor (stress intensity factor) per Newman-Raju (1981) for semi-elliptical surface cracks; used in windshield failure analysis |

## Key Modules Exported
- **`newman_raju_mod`** — `newman_raju(…)` subroutine

## Dependencies
- Uses: `modules/precision_mod.F90` (precision `WP`)
- Used by: glass/windshield failure law implementations in the engine
