# engine/source/coupling/

## Purpose
External co-simulation coupling adapters: allows OpenRadioss to exchange data
with other solvers (e.g., VIPER/Altair fluid solver, preCICE framework) or to
accept Python-based coupling. This is a plugin-style layer — the core engine
calls abstract coupling interfaces; concrete adapters are compiled in or stubbed
out at build time.

## Files

| File | Role |
|------|------|
| `coupling_adapter.F90` | Fortran abstract adapter type: `init`, `exchange`, `finalize` interface |
| `coupling_factory.cpp` / `.h` | C++ factory: selects and instantiates the correct adapter at runtime |
| `coupling_c_interface.cpp` / `.h` | C bridging layer between Fortran and C++ adapters |
| `coupling.h` | Shared C++ coupling header |
| `README.md` | Brief description of the coupling architecture |

## Sub-directories

| Sub-dir | Description |
|---------|-------------|
| `viper/` | VIPER (Altair fluid) coupling adapter: `viper_interface_mod.F90` + C++ implementation |
| `precice/` | preCICE open-source coupling library adapter: `precice_coupling_adapter.cpp/h` |
| `python/` | Python-driven co-simulation coupling |
| `rad2rad/` | Radioss-to-Radioss coupling (domain overlapping/embedding) |
| `dummy_coupling_adapter.cpp/h` | No-op stub used when no external coupling is configured |

## Integration
Coupling is activated by the `/COUPLING` or `/CWIPI` keywords in the engine input
deck. The coupling `exchange` call happens in `RESOL` at a configurable time
frequency, transferring pressure/displacement data to/from the partner solver.

## Dependencies
- Called by: `RESOL` (via `FRECPL` reader → coupling setup in `radioss2.F`)
- Uses: `spmd_mod` for MPI-aware data exchange when co-simulation spans MPI domains
