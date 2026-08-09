# Contact Sandbox — Source (`tools/mockup/contact_sandbox/source/`)

Fortran source files implementing TYPE7 broad-phase contact detection, extracted
from `engine/source/interfaces/intsort/`.  The harness runs candidate search and
penetration filtering on a pre-loaded dataset (`t10m.dat`) and reports timing.

## Key Files

| File | Role |
|------|------|
| `collision_mod.F` | `COLLISION_MOD` — defines `GROUP_SIZE = 64` vectorisation parameter |
| `inter7_candidate_pairs.F` | `INTER7_CANDIDATE_PAIRS` — voxel broad-phase: builds (slave node, master segment) candidate list |
| `inter7_filter_cand.F` | `INTER7_FILTER_CAND` — narrow-phase filter: removes geometrically impossible pairs |
| `inter7_gather_cand.F` | `INTER7_GATHER_CAND` — gathers surviving candidates into INTBUF format |
| `inter7_penetration.F` | `INTER7_PENETRATION` — computes penetration depth and normal for each candidate pair |
| `unit_test1.F` | Main driver: reads dataset, calls detection loop, prints elapsed time |
| `compare_cand.cpp` | C++ reference result comparison helper |
| `unlimit_stack.cpp` | Sets unlimited stack size at startup (needed for large Fortran arrays) |
| `constant_mod.F` | Physical / numerical constants module |

## Related Documentation

- `tools/mockup/contact_sandbox/README.md` — build and run instructions (parent directory)
- `tools/mockup/contact_sandbox/source/include/README.md` — include files
- `engine/source/interfaces/intsort/README.md` — production version
