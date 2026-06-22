# Airbag Subsystem

This subsystem implements airbag and control-volume (gas-filled volume) models. Two distinct physical approaches are available: the **control-volume (CV)** model and the **finite-volume (FV)** model.

Activated by `/MONVOL` (control-volume) and `/MONVOL/FVMBAG` (finite-volume) keywords.

## Directory Structure

All airbag source files reside directly in `airbag/` (no subdirectories).

## Control-Volume (CV) Model — `monvol*`, `volum*`, `volpres*`

The control-volume approach treats the airbag as a single uniform-pressure gas volume. The bag pressure is updated from the gas thermodynamics and the bag volume evolution.

| File | Role |
|------|------|
| `monvol0.F` | CV main loop — pressure update, volume integration |
| `monv_imp0.F` | Implicit CV pressure solver |
| `volum0.F` | Volume computation from mesh |
| `volpres.F` | Pressure force application to bag surface |
| `volpresp.F` | Pressure application with porosity |
| `volpvg.F` | Pressure application for vented gas |
| `volp_lfluid.F` | Pressure for liquid-filled volumes |
| `volout.F` | CV output writing |
| `mvoludt.F` | CV time step contribution |
| `monvol0.F` | Initialisation |
| `rbagdt.F` | Rigid bag time step |

### CV Gas Model

The bag pressure `P` satisfies a thermodynamic state equation updated each step:
```
P·V = n·R·T   (ideal gas)   or   a tabulated state equation
```
The volume `V` is computed from the current mesh geometry. Gas influx (jetting), venting through fabric, and heat exchange with the surroundings are included.

## Finite-Volume (FV) Model — `fvbag*`, `fvbric*`

The FV model resolves the gas flow inside the bag on a moving mesh. It captures jetting, pressure waves, and non-uniform pressure distributions that the CV model cannot.

| File | Role |
|------|------|
| `fvbag0.F` | FV initialisation |
| `fvbag1.F` | FV main time step |
| `fvbag2.F` | FV gas dynamics integration |
| `fvbric.F` | FV brick mesh operations |
| `fvmesh.F`, `fvmesh0.F` | FV mesh management |
| `fvrezone.F` | FV mesh rezoning |
| `fvcopy.F` | FV array copy utilities |
| `fvdim.F` | FV array dimension management |
| `fvdeal.F` | FV memory deallocation |
| `fvupd.F` | FV state update |
| `fvtemp.F` | FV temperature computation |
| `fvstats.F`, `fvstats1.F` | FV statistics / diagnostics |
| `fvinjt6.F`, `fvinjt8.F`, `fvinjt8_1.F` | FV injector models (gas jetting) |
| `fvvent0.F` | FV venting model |
| `fv_up_switch.F` | Switch between FV update modes |

## Gas Influx (Injectors) — `airbag1.F`, `airbag2.F`, `airbaga1.F`, `airbagb1.F`

These files model the inflator — the gas generator that inflates the bag. The influx may be specified as a mass flow rate or a pressure-volume curve.

## Porosity / Venting — `porfor*.F`

| File | Role |
|------|------|
| `porfor4.F` | Porosity force for vent holes (TYPE4) |
| `porfor5.F` | Porosity force (TYPE5: Wang-Nefske) |
| `porfor6.F` | Porosity force (TYPE6: Graefe model) |

Fabric porosity (gas leakage through the woven fabric) is modelled in `volpresp.F` and `fvvent0.F`.

## Volume / Area Utilities

| File | Role |
|------|------|
| `get_volume_area.F90` | Compute enclosed volume and surface area from mesh |
| `mhvis3.F` | Visual mesh helper for bag geometry |
| `roto.F`, `uroto.F` | Rotation operations for bag orientation |

## MPI Considerations

For parallel runs, the bag volume and pressure must be globally reduced across all MPI ranks. `init_global_monvol_frontier.F90` and `init_monvol_omp_structure.F90` manage the parallel data structures for this reduction.

## Related Documentation

- `engine/source/boundary_conditions/README.md` — pressure BCs applied to bag surfaces
- `common_source/eos/README.md` — gas EOS models used inside the bag
