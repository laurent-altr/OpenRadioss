# ALE Subcycling (`engine/source/ale/subcycling/`)

Implements subcycling for ALE/Euler elements — running the fluid time integration at a different (larger) time step than the structural elements.

## Key Files

| File | Role |
|------|------|
| `alesub1.F` | ALE subcycling stage 1: accumulate fluxes over multiple structural time steps |
| `alesub2.F` | ALE subcycling stage 2: apply accumulated fluxes to ALE variables |
| `aleconv.F` | ALE convection during subcycling step |
| `aleflow.F` | ALE flow rate tracking during subcycling |
| `aleflux.F` | ALE flux accumulation across subcycles |

## Subcycling Motivation

ALE/Euler elements often have a much larger stable time step than fine structural elements (fluid wave speeds are lower than structural wave speeds for many materials). Subcycling allows:
- Structural elements: advance `N` steps at `dt_struct`
- ALE elements: advance 1 step at `N × dt_struct`

This reduces the ALE computation cost by factor `N` while keeping the structural integration accurate.

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
- `engine/source/time_step/README.md` — time step computation
