# QA Utilities (common_source/qa)

This directory provides quality-assurance support utilities shared between the starter and engine.

## Files

| File | Role |
|------|------|
| `qa_out_mod.F` | `QA_OUT_MOD` — Fortran module for writing QA-check output lines |
| `kill_zombi.F` | Detects and removes "zombie" elements (elements with zero or negative volume that would cause numerical failure) |

## QA Output Module (`qa_out_mod.F`)

`QA_OUT_MOD` provides the data structures and write routines for the QA-print table — the per-keyword summary that appears in the `.out` file when `/QAPRINT` is active. The table format is:

```
/KEYWORD/TYPE
  Parameter1     Value1     Unit
  Parameter2     Value2     Unit
```

This module is used by both the starter (to document the model setup) and the engine (for intermediate QA checks during the run).

## Zombie Element Detection (`kill_zombi.F`)

In crash/impact simulations, elements can become severely distorted (negative Jacobian, zero volume). These "zombie" elements produce NaN/Inf forces and must be deactivated. `kill_zombi.F` scans all elements each step and sets the `OFF` flag to 0 for any zombie, removing it from the force computation.

This is separate from material failure (`OFF=0` from failure criteria in `materials/fail/`) — zombie detection triggers on geometric degeneracy regardless of material state.

## Related Documentation

- `engine/source/output/qaprint/` — engine-side QA print output formatting
- `engine/source/materials/README.md` — material failure (sets `OFF` flag via failure criteria)
