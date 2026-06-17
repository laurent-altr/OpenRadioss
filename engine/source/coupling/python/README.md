# Python Coupling (`engine/source/coupling/python/`)

Provides a Python scripting interface for airbag control volume models and shared-memory inter-process coupling.

## Key Files

| File | Role |
|------|------|
| `python_monvol.F90` | Python-driven control volume (monvol) airbag: call Python script to compute pressure/temperature |
| `python_share_memory.F90` | Shared-memory coupling with a Python co-process: set up and read/write shared memory segments |

## Python Monvol

The Python control volume interface allows users to define airbag thermodynamics in Python rather than the built-in OpenRadioss gas law. Each time step:
1. OpenRadioss calls the Python interpreter (via embedded CPython)
2. Passes current volume, temperature, gas composition
3. Python computes new pressure, temperature, vent flow rates
4. Returns values to OpenRadioss

This is activated by `/MONVOL/PYBAG` or a `/MAT/GAS` with a Python law reference.

## Shared Memory Coupling

`python_share_memory.F90` implements a co-simulation interface through POSIX shared memory. A Python co-process (e.g., a controls model, sensor processing script) can read simulation state and write back control inputs at each coupling step without network overhead.

## Related Documentation

- `engine/source/coupling/README.md` — parent coupling directory (preCICE/CWIPI)
- `engine/source/airbag/README.md` — control volume airbag models
- `common_source/modules/README.md` — `python_mod.F90` Python integration module
