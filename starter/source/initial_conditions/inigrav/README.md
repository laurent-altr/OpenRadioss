# Initial Gravity Equilibrium (`starter/source/initial_conditions/inigrav/`)

Reads /INIGRAV initial gravity-equilibrium state: pre-stress and pre-deformation from a gravitational loading phase applied before `t=0`.

## Key Files

| File | Role |
|------|------|
| `hm_read_inigrav.F` | Parse /INIGRAV card: gravity vector, load curve, target equilibrium time |
| `inigrav_load.F` | Apply incremental gravity load during pre-simulation phase |
| `inigrav_eos.F` | Initialise EOS state under gravity-induced pressure |
| `inigrav_m37.F` | Gravity equilibrium for LAW37 (fabric) material |
| `inigrav_m51.F` | Gravity equilibrium for LAW51 (composite) material |
| `inigrav_part_list.F` | Build list of parts subject to gravity initialisation |

## Description

`/INIGRAV` performs a static gravity-equilibration step before the main analysis: the gravity load is ramped up, the model is brought to equilibrium (with optional damping), and the resulting stress/strain state is stored as the initial condition. Material-specific routines (`inigrav_m37.F`, `inigrav_m51.F`) handle specialised constitutive updates during this pre-step.

## Related Documentation

- `starter/source/initial_conditions/README.md` — parent directory
- `engine/source/loads/general/grav/README.md` — runtime gravity body force
