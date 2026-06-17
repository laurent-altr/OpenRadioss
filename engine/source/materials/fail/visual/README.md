# Visual Failure Criterion (`engine/source/materials/fail/visual/`)

Failure criterion that provides a visual damage indicator without element deletion: outputs a damage field for post-processing without modifying element stiffness.

## Key Files

| File | Role |
|------|------|
| `fail_visual_c.F` | Visual damage indicator for solid elements |
| `fail_visual_s.F` | Visual damage indicator for shell elements |
| `fail_visual_b.F90` | Visual damage indicator for beam elements |
| `fail_visual_ib.F90` | Visual damage indicator for implicit beam elements |

## Description

`visual` computes and outputs a scalar damage index `D ∈ [0, 1]` each step based on the chosen criterion (strain, stress, or energy-based threshold) without removing elements or reducing stiffness. This allows analysts to:

- Visualise proximity to failure in H3D/animation output
- Identify critical regions without model changes
- Compare multiple criteria simultaneously on the same model

Useful during design iteration where premature element deletion would obscure the deformation pattern.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/output/h3d/README.md` — H3D output for visualisation
