# engine/source/general_controls/

## Purpose
General simulation control features that operate globally across the model:
structural damping, stochastic noise perturbation (for symmetry breaking), and
statics/quasi-static controls. These are activated by `/DAMP`, `/DYREL`, or
noise-injection keywords in the engine input deck.

## Sub-directories

| Sub-dir | Contents |
|---------|----------|
| `damping/` | Frequency-range damping: `DAMPING_RANGE_SOLID`, `DAMPING_RANGE_SHELL`, `DAMPING_RANGE_SHELL_MOM` — apply Maxwell-model damping in selected frequency bands; `STATIC` — static/quasi-static damping scheme |
| `computation/` | Noise injection for symmetry breaking: `NOISE` (main), `INITNOISE` (initialization), `LECNOISE` (input reader), `PNOISE` (print/output of noise parameters) |

## Integration in the cycle
- Damping routines in `damping/` provide per-group `DAMP_RANGE` buffer parameters
  (stored in `ELBUF_TAB(NG)%DAMP_RANGE`); they contribute additional damping
  forces inside element kernels.
- Noise routines inject random velocity perturbations at the start of the run
  (used for bifurcation analysis in buckling problems).

## Dependencies
- Called by: `RESOL_INIT` (noise), element kernels (damping)
- Uses: `ELBUF_STRUCT_` `damp_range` field (see `doc/ELBUF_TAB_documentation.md`)
