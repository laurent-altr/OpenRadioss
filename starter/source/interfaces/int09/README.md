# TYPE9 Interface — Tied with Failure (`starter/source/interfaces/int09/`)

Starter reader for /INTER/TYPE9: tied node-to-surface contact with cohesive failure (delamination).

## Key Files

| File | Role |
|------|------|
| `hm_read_inter_type09.F` | Parse /INTER/TYPE9 card parameters |
| `i9bcs_check.F90` | Check BCs compatibility for TYPE9 tied-with-failure nodes |
| `i9sti2.F` | 2D stiffness initialisation for TYPE9 |
| `i9sti3.F` | 3D stiffness initialisation for TYPE9 |

## Description

TYPE9 extends TYPE2 tied contact with a cohesive zone failure criterion. When the interfacial traction exceeds the tensile or shear strength, the tie breaks and the interface transitions to sliding contact. The starter reads the strength parameters, mixed-mode failure criterion, and energy release rates.

## Related Documentation

- `starter/source/interfaces/reader/README.md` — interface dispatcher
- `starter/source/interfaces/int02/README.md` — TYPE2 tied contact
