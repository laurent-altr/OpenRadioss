# Time Step Utilities (`common_source/tools/time_step/`)

Shared time-step computation utilities used by both starter and engine.

## Key Files

| File | Role |
|------|------|
| `find_dt_target.F` | Compute the target time step needed to achieve a given added-mass fraction (used by AMS/SMS auto-DT) |

## Purpose

`find_dt_target.F` implements the inverse of the element time-step function: given a desired global time step `dt_target`, it computes the mass scaling factor required to make all elements stable at that step. This is the core computation for Selective Mass Scaling (`/DT/AMS`) automatic mode.

The routine works by iterating over all elements, computing their natural time step `dt_elem`, and accumulating the mass addition `Δm = m × (dt_elem/dt_target)² - m` for elements where `dt_elem < dt_target`.

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `engine/source/ams/README.md` — AMS/SMS engine-side implementation
- `engine/source/time_step/README.md` — per-element time step computation
- `starter/source/ams/README.md` — AMS initialisation in starter
