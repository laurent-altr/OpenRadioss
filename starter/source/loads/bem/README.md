# BEM Load (`starter/source/loads/bem/`)

Reads /BEM (Boundary Element Method) load definitions for acoustic radiation and structural-acoustic coupling.

## Key Files

| File | Role |
|------|------|
| `hm_read_bem.F` | Parse /BEM card: acoustic surface, frequency range, fluid properties |

## Description

`/BEM` sets up a boundary element method acoustic analysis coupled to the FEM structural model. The starter reads the fluid surface definition, acoustic domain properties, and frequency parameters, writing the coupling data to the restart file.

## Related Documentation

- `starter/source/loads/README.md` — parent directory
