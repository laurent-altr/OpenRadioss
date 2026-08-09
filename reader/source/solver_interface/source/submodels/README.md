# Solver Interface — Submodels (`reader/source/solver_interface/source/submodels/`)

Fortran-callable wrappers that build and query the submodel (include-file)
hierarchy inside the SDI model.

## Key Files

| File | Role |
|------|------|
| `cpp_submodel_build.cpp` | `cpp_submodel_build_` — constructs one submodel node: sets parent (`IFATHER`), nesting level, ID offsets, and UID |
| `cpp_submodel_count.cpp` | Return total number of submodels (include levels) |
| `cpp_nodes_subtag.cpp` | Annotate nodes with their submodel tag |
| `cpp_shell_subtag.cpp` | Annotate shell elements with their submodel tag |

## Related Documentation

- `reader/source/solver_interface/source/README.md` — parent directory
