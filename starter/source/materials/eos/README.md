# Starter EOS Input (`starter/source/materials/eos/`)

Reads Equation of State (EOS) parameters for materials that require pressure-density-energy relationships.

## Key Files

| File | Role |
|------|------|
| `hm_read_eos.F` | Main EOS reader: dispatch to per-type readers |
| `hm_read_eos_compaction.F90` | Read compaction EOS (porous material pressure-density) |
| `hm_read_eos_compaction2.F90` | Read compaction EOS TYPE2 (extended Herrmann model) |
| `hm_read_eos_compaction_tab.F90` | Read tabulated compaction EOS |

## Description

EOS types read by `hm_read_eos.F` include:
- Gruneisen, JWL (Jones-Wilkins-Lee), Tillotson, NASG (Noble-Abel stiffened gas)
- Ideal gas, tabulated EOS, linear EOS
- Compaction EOS for porous media (powder metallurgy, soil)

EOS parameters are stored in the `eos_param_mod` module and written to the restart file for the engine.

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `common_source/eos/README.md` — EOS implementations
- `common_source/modules/mat_elem/README.md` — eos_param_mod data structure
