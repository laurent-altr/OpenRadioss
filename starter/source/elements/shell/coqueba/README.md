# BT Shell Initialisation (`starter/source/elements/shell/coqueba/`)

Starter initialisation for 4-node Belytschko-Tsay (BT) shell elements.

## Key Files

| File | Role |
|------|------|
| `cbainit3.F` | Main BT shell element initialisation |
| `cbafint_reg_ini.F` | Initialise BT regular integration point layout |
| `cndleni.F` | Compute BT nodal characteristic length |
| `cnepsini.F` | Initialise BT strain state |
| `cneveci.F` | Compute BT shell eigenvectors |
| `cstraini4.F` | Initialise 4-node shell strain |
| `cuserini4.F` | Initialise user material state for BT shells |
| `layini1.F` | Initialise layer (ply) state for composite BT shells |
| `scigini4.F` | Initialise BT shell stress state |
| `thickini.F` | Initialise through-thickness integration state |

## Related Documentation

- `starter/source/elements/shell/README.md` — parent shell directory
- `engine/source/elements/shell/coqueba/README.md` — BT engine integration
