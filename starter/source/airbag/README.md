# Starter Airbag Subsystem

This subsystem reads and initialises airbag and control-volume definitions.

## Key Files

| File | Role |
|------|------|
| `fvbric.F`, `fvbric0.F`, `fvbric1.F`, `fvbric2.F`, `fvbric01.F` | FV airbag brick mesh initialisation |
| `c_fvbag.F` | C bridge for FV bag data |
| `facepoly.F` | Face polygon computation for FV mesh |
| `Connectivity.cpp` | C++ connectivity builder for FV bag mesh |

## Two Airbag Models

As with the engine (see `engine/source/airbag/README.md`), two approaches are initialised here:

### Control-Volume (`/MONVOL`)
Reads control-volume bag parameters:
- Gas model (ideal gas, tabulated)
- Inflator mass flow / pressure curve
- Vent area curve
- Bag surface group (shell elements forming the bag)

The CV model is simple to initialise — just parameter reading and group validation.

### Finite-Volume (`/MONVOL/FVMBAG`)
Much more complex initialisation: the starter builds the internal FV mesh from the bag's shell surface:
- `Connectivity.cpp` — topological connectivity of the FV brick cells
- `fvbric*.F` — geometry of each FV brick (volume, face areas, normals)
- `facepoly.F` — polygon decomposition of each cell face

The FV mesh is stored in the restart file for the engine to use at runtime.

## Related Documentation

- `engine/source/airbag/README.md` — runtime airbag computation
