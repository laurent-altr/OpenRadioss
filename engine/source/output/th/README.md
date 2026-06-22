# Time-History Output (`engine/source/output/th/`)

Writes `.th` time-history ASCII/binary files: per-entity scalar quantities sampled at high frequency (every few steps) for plotting in HyperGraph.

## Key Files

| File | Role |
|------|------|
| `init_th.F` | Initialise TH file structure at start of run |
| `init_th0.F` | Initialise TH file at restart |
| `init_th_group.F` | Initialise TH for node/element groups |
| `write_th.F` | Write TH data for the current output step |
| `write_th_restart.F` | Write TH data to restart file |
| `read_th_restart.F` | Read TH data from restart file |
| `hist1.F` / `hist2.F` | Node history: displacements, velocities, accelerations |
| `hist13.F` | Node history for beam nodes |
| `thcoq.F` / `thsol.F` / `thsph.F` | Shell/solid/SPH element history |
| `thcoq_count.F` | Count TH output quantities for shells |
| `thnod.F` / `thnod_count.F` | Nodal TH output |
| `thmonv.F` | Monvol (airbag) TH: volume, pressure, temperature |
| `thsens.F` | Sensor time history |
| `bcs1th.F` / `thbcs.F` | Boundary condition TH: reaction forces |
| `thres.F` | Resultant forces on a surface TH |
| `surf_area.F` | Surface area computation for area-averaged TH |
| `thchecksum.F90` | Checksum for TH repeatability testing |

## Related Documentation

- `engine/source/output/README.md` — all output types
- `engine/source/output/h3d/README.md` — H3D full-field output
- `starter/source/output/th/README.md` — TH configuration in starter
