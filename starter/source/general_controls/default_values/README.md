# Default Values (`starter/source/general_controls/default_values/`)

Reads /DEF_INTER and /DEF_SHELL, /DEF_SOLID cards that set global default parameters for interface, shell, and solid element formulations.

## Key Files

| File | Role |
|------|------|
| `hm_read_definter.F` | Parse /DEF_INTER: global contact default parameters |
| `hm_read_definter_type02.F` | Defaults for TYPE2 tied contact |
| `hm_read_definter_type07.F` | Defaults for TYPE7 general contact |
| `hm_read_definter_type11.F` | Defaults for TYPE11 edge-to-edge contact |
| `hm_read_definter_type19.F` | Defaults for TYPE19 solid-to-solid contact |
| `hm_read_definter_type24.F` | Defaults for TYPE24 surface-to-surface contact |
| `hm_read_definter_type25.F` | Defaults for TYPE25 edge-to-surface contact |
| `hm_read_defshell.F` | Parse /DEF_SHELL: default shell formulation, hourglass, thickness update |
| `hm_read_defsolid.F` | Parse /DEF_SOLID: default solid formulation, hourglass, bulk viscosity |

## Description

These cards allow a deck-level override of element and contact defaults so that individual property cards need not repeat common parameters. `hm_read_defshell.F` sets defaults for shell formulation flag (QEPH, BT, DKT), integration rule, and hourglass coefficient. `hm_read_defsolid.F` sets defaults for solid formulation (one-point, 8-point), hourglass, and bulk viscosity. The `/DEF_INTER` family sets default values for gap, Coulomb friction, and penalty coefficients per contact type.

## Related Documentation

- `starter/source/general_controls/README.md` — parent directory
- `starter/source/properties/README.md` — property/section readers
- `starter/source/interfaces/README.md` — interface readers
