# Starter SPH Loads (`starter/source/loads/sph/`)

Reads SPH inlet/outlet boundary conditions (SPH injection and outlet definitions).

## Key Files

| File | Role |
|------|------|
| `hm_read_sphio.F` | Read SPH inlet/outlet: inflow velocity, material, injection rate |
| `hm_preread_sphio.F` | Pre-read pass for array sizing |

## Description

SPH inlet/outlet BCs allow particles to enter (`/INFLOW`) or leave (`/OUTFLOW`) the SPH domain, enabling free-surface flows and fluid-jet simulations. `hm_read_sphio.F` reads the inlet/outlet geometry (defined by a segment set), flow parameters (velocity, density, material), and links to time-function curves.

## Related Documentation

- `starter/source/loads/README.md` — parent loads directory
- `engine/source/elements/sph/README.md` — SPH element in engine
