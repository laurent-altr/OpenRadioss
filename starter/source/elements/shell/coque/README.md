# QEPH Shell Initialisation (`starter/source/elements/shell/coque/`)

Starter initialisation for 4-node QEPH (Quasi-Exact Physical Hourglass) shell elements.

## Key Files

| File | Role |
|------|------|
| `cinit3.F` | Main QEPH shell element initialisation |
| `cinmas.F` | Compute shell nodal mass |
| `c1buf3.F` | Allocate QEPH element buffer (ELBUF) |
| `ccoli3.F` | Shell element collinearity check |
| `ccoori.F` | Compute shell local coordinate system |
| `cderii.F` | Compute shape function derivatives |
| `cdleni.F` | Compute shell characteristic length for DT |
| `cepsini.F` | Initialise strain state |
| `ceveci.F` | Compute shell eigenvectors |
| `cfailini.F` | Initialise failure state |
| `cfint_reg_ini.F` | Initialise regular integration point layout |
| `cgrhead.F` | Write QEPH shell group header to restart |
| `cgrtails.F` | Write QEPH shell group tail data to restart |
| `clskew.F` | Set shell local skew frame |
| `cm1inif.F` | Initialise M1 (membrane) integration |
| `corth3.F` | Compute orthogonal shell frame |
| `corthdir.F` | Set orthotropic material direction |
| `corthini.F` | Initialise orthotropic frame |
| `csigini.F` | Initialise stress state |
| `cuserini.F` | Initialise user material state |
| `cveok3.F` | Check shell element volume |
| `lcgeo19.F` | Shell geometry check (warning if distorted) |
| `set_elgroup_param.F` | Set element group parameters |

## Related Documentation

- `starter/source/elements/shell/README.md` — parent shell directory
- `engine/source/elements/shell/coque/README.md` — QEPH engine integration
