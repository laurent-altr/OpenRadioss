# OpenRadioss-to-OpenRadioss Coupling (`engine/source/coupling/rad2rad/`)

Implements direct coupling between two running OpenRadioss instances — a global model and a submodel running simultaneously.

## Key Files

| File | Role |
|------|------|
| `r2r_init.F` | Initialise rad2rad coupling: open inter-process communication channel |
| `r2r_input_init.F` | Initialise input data structures for data received from partner |
| `r2r_exchange.F` | Exchange coupling data each time step: send boundary displacements, receive forces |
| `r2r_getdata.F` | Extract coupling quantities from the OpenRadioss data arrays |
| `rad2rad_c.c` | C-level transport layer: socket or named-pipe communication |
| `sys_pipes_c.c` | System pipe utilities for inter-process data transfer |

## Use Case

`rad2rad` coupling allows a coarse global model to drive the boundary conditions of a fine local model in real time:

```
Global OpenRadioss ←→ rad2rad channel ←→ Local OpenRadioss
   (coarse mesh)          (pipes/sockets)      (fine mesh)
```

At each coupling time step:
1. Global model sends boundary node displacements to local model
2. Local model sends reaction forces back to global model
3. Both models advance their time integration

This is tighter than the submodel approach (which reads global results from a file after the fact) — both models advance simultaneously.

## Related Documentation

- `engine/source/coupling/README.md` — parent coupling directory (preCICE, CWIPI)
- `starter/source/coupling/README.md` — starter rad2rad coupling setup
- `starter/source/model/submodel/README.md` — file-based submodel (different approach)
