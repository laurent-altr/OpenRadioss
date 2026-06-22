# Boundary Conditions (`starter/source/constraints/general/bcs/`)

Reads and initialises translational and rotational boundary conditions (/BCS): fixed DOFs, imposed displacements, cyclic symmetry.

## Key Files

| File | Role |
|------|------|
| `hm_read_bcs.F` | Parse /BCS card: DOF mask, node/part selector |
| `hm_read_nbcs.F` | Parse /NBCS (named BCS): named boundary condition set |
| `lecbcscyc.F` | Read cyclic symmetry BCS from legacy restart |
| `printbcs.F` | Print BCS summary to starter output |

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `starter/source/constraints/general/impvel/README.md` — imposed velocity BCS
