# VIPER Interface (`engine/source/coupling/viper/`)

Interface to the VIPER (Vehicle Integrated Platform for Engineering and Research) co-simulation framework.

## Key Files

| File | Role |
|------|------|
| `viper_interface_mod.F90` | Fortran module implementing the VIPER coupling API: send/receive field data, synchronise time steps |

## VIPER Coupling

VIPER is an Altair-internal co-simulation bus that connects multiple solvers (structural, fluid, controls, thermal) during a single simulation. OpenRadioss registers as a VIPER participant and:
1. Sends structural displacements/forces to the VIPER bus at each coupling step
2. Receives forces or pressures from other participants (e.g., fluid pressure from a CFD solver)
3. Synchronises time steps with the VIPER master

This is the Altair-proprietary alternative to the open preCICE adapter. Users with Altair HyperWorks infrastructure use VIPER; open-source users use preCICE.

## Related Documentation

- `engine/source/coupling/README.md` — parent coupling directory (preCICE, CWIPI adapters)
