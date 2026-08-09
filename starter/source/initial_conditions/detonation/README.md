# Detonation Initial Conditions (`starter/source/initial_conditions/detonation/`)

Sets up explosive detonation initiation: detonation points, lines, planes, cord, and screen definitions plus the Eikonal fast-marching solver that propagates the detonation front.

## Key Files

| File | Role |
|------|------|
| `prelecdet.F` | Pre-read: allocate detonator data structures |
| `read_detonators.F` | Main reader for /DETONATOR card |
| `read_dfs_detpoint.F` | Point detonator geometry reader |
| `read_dfs_detline.F` | Line detonator geometry reader |
| `read_dfs_detplan.F` | Planar detonator geometry reader |
| `read_dfs_detcord.F` | Detonation cord reader |
| `read_dfs_wave_shaper.F` | Wave-shaper (barrier) reader |
| `detcord.F` | Cord detonation timing: propagate burn front along cord |
| `detcord0.F` | Detonation cord initialisation |
| `ecran1.F` | Screen (planar barrier) detonation front intersection |
| `ecran2.F` | Screen detonation timing for each element |
| `m5in2.F` | 2D detonation initialisation |
| `m5in2t.F` | 2D detonation time array setup |
| `m5in3.F` | 3D detonation initialisation |
| `iombr.F` | Element shadow computation for wave-shaper |
| `eikonal_solver.F90` | Fast Marching Method (FMM) Eikonal solver entry |
| `eikonal_fast_marching_method.F90` | Core FMM loop: narrow-band heap update |
| `eikonal_godunov_operator_2d/3d.F90` | Upwind Godunov operator for 2D/3D Eikonal |
| `eikonal_ini_mixture_vel.F90` | Set detonation velocity per element (mixed materials) |
| `eikonal_init_start_list.F90` | Seed the FMM with initial detonation front nodes |
| `eikonal_compute_adjacent.F90` | Build node adjacency for FMM |
| `eikonal_bcs_sym_tag.F90` | Symmetry boundary conditions for FMM |
| `eikonal_Lmax.F90` | Compute maximum characteristic length for FMM |
| `detonation_times_printout.F90` | Write detonation time array to starter output |
| `unused_mat_detonator.F` | Check for materials not referenced by any detonator |

## Algorithm

Detonation arrival time `T(x)` at each node is the solution to the Eikonal equation:

```
|∇T| = 1/D(x)
```

where `D(x)` is the local detonation velocity (from material or cord definition). The Fast Marching Method (`eikonal_fast_marching_method.F90`) propagates the front from seed nodes using the Godunov upwind operator, processing nodes in order of increasing arrival time via a min-heap. The resulting per-element burn time drives the JWL EOS ignition in the engine.

## Related Documentation

- `starter/source/initial_conditions/README.md` — parent directory
- `engine/source/materials/mat/mat005/` — JWL explosive material law
- `common_source/eos/README.md` — JWL equation of state
