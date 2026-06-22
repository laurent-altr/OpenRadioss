# Model Analysis Output (`starter/source/output/analyse/`)

Generates the model analysis report — a structured summary of the input deck written to the `.out` file. Catches input errors and prints warnings before writing the restart file.

## Key Files

| File | Role |
|------|------|
| `analyse.c` | Main analysis driver: orchestrate all analysis passes |
| `analyse_structure.c` | Structural analysis: check element quality, detect degenerate elements |
| `analyse_mat.c` | Material validation: check parameter ranges, flag invalid combinations |
| `analyse_node.c` | Node analysis: check for isolated nodes, duplicate coordinates |
| `analyse_part.c` | Part analysis: check element-property-material consistency |
| `analyse_check.c` | General checks: missing required inputs, ID conflicts |
| `analyse_memory.c` | Memory usage report |
| `analyse_out.F` | Fortran output: write analysis summary to `.out` file in formatted tables |
| `analyse_error.c` | Error accumulation and final error/warning count |
| `analyse_char.F` | Character/string analysis utilities |
| `analyse_real.F` | Real number analysis utilities |
| `analyse_comment.c` | Handle commented-out sections in analysis output |
| `analyse_fill_info.c` | Fill analysis info structures from model data |
| `analyse_getall.c` | Collect all analysis results into summary structure |
| `analyse_print.c` | Print formatted analysis tables |
| `analyse_read_tools.c` | Read and format analysis tool output |
| `analyse_string.c` | String utilities for analysis output |
| `analyse_arret.F` | Call `ARRET` if analysis found fatal errors |
| `analyse_GUI.c` | Output analysis results in GUI-parseable format (for HyperMesh feedback) |

## Analysis Checks

The analyse subsystem performs model validation before writing the restart file:

1. **Node checks**: isolated nodes (connected to no element), duplicate coordinates
2. **Element checks**: degenerate elements (zero volume/area), badly distorted elements (aspect ratio)
3. **Material checks**: physically invalid parameters (negative density, E=0)
4. **Part checks**: elements referencing non-existent property or material IDs
5. **Contact checks**: interpenetrating surfaces in initial configuration

Warnings are printed to `.out`; fatal errors call `ARRET` and stop the run.

## Related Documentation

- `starter/source/output/README.md` — parent output directory
- `common_source/qa/README.md` — zombie element detection (related concept)
