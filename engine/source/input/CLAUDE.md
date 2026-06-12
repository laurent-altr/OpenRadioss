# engine/source/input/

## Purpose
Engine input reading: parses the engine `.rad` keyword deck and reads the
domain restart files (`_NNNN.rst`). Called once during startup from `RADIOSS2`
before the time loop begins.

## Key files

| File | Role |
|------|------|
| `lecinp.F` | Top-level engine keyword reader: opens engine input and dispatches `FRE*` routines |
| `lecstat.F` | Reads static/structural data from restart file (node coords, connectivity, groups) |
| `lectur.F` | Reads time-dependent state from restart file (velocities, stresses, `ELBUF_TAB`) |
| `rdele.F` | Reads element deletion (erosion) state from restart |
| `lecfvbag.F`, `lecfvbag1.F` | Reads FV airbag data from restart |
| `lecfxinp.F` | Reads fixed-body input data |
| `leceig.F` | Reads eigenmode data |
| `lecinv.F` | Reads initial velocity data |
| `lecdamp.F` | Reads damping data |
| `manctr.F` | Manual control / trigger keyword processing |
| `ixyz.F` | Reads nodal coordinate array from restart |
| `read5p.F`, `read10.F`, `read10p.F` | Low-level Fortran unformatted restart file readers |
| `redkey0.F`, `redkey1.F`, `redkey2.F` | Keyword search and dispatch in engine deck |
| `redkey1_h3d.F` | H3D-specific keyword reader |
| `wciusc2.F`, `wriusc2.F` | Write / read ASCII format converters |
| `errmsg.F` | Error message formatting for input errors |

## `FRE*` keyword readers (called from `LECINP`)

Each `/KEYWORD/` in the engine input deck is dispatched to a `FRE*` routine:

| Routine | File | Keyword processed |
|---------|------|------------------|
| `FREBCS` | `frebcs.F` | `/BCS` boundary conditions |
| `FRECPL` | `frecpl.F` | `/COUPLING` co-simulation |
| `FREDAMP` | `fredamp.F` | `/DAMP` damping |
| `FREDYNAIN` | `fredynain.F` | `/DYNAIN` dynain restart |
| `FREEIG` | `freeig.F` | `/EIGE` eigenmode |
| `FREFLW` | `freflw.F` | `/FLOW` flow boundary |
| `FREFORM` | `freform.F` | `/FORM` element formulation override |
| `FREFUNC` | `frefunc.F` | `/FUNCT` time function tables |
| `FREFVBAG` | `frefvbag.F` | `/MONVOL` airbag parameters |
| `FREFXINP` | `frefxinp.F` | `/FIXBODY` fixed body |
| `FREIMPL` | `freimpl.F` | `/IMPL` implicit parameters |
| `FREINIV` | `freiniv.F` | `/INIV` initial velocity |
| `FREINT` | `freint.F` | `/INTER` interface definition |
| `FRELNK` | `frelnk.F` | `/LINK` node linking |
| `FRENOIS` | `frenois.F` | `/NOISE` noise parameters |
| `FREOUTP` | `freoutp.F` | `/ANIM`, `/TH`, `/PRINT`, `/H3D` output triggers |
| `FRERBO` | `frerbo.F` | `/RBODY` rigid body |
| `FRESTAT` | `frestat.F` | `/DT` time step, `/STOP` stop criteria |
| `FRALEONOFF` | `fraleonoff.F` | `/ALE` on/off switching |
| `FRALNK` | `fralnk.F` | `/ALE` link parameters |
| `FREUPWIND` | `freupwm.F`, `freupwind.F` | `/UPWIND` ALE scheme |
| `FREDEBUG` | `fredebug.F` | `/DEBUG` debug flags |

## Dependencies
- Called by: `RADIOSS2` (`engine/source/engine/radioss2.F`)
- Produces: fully populated `NODES`, `ELBUF_TAB`, `IPARG`, `IXS`/`IXC`/…, group structures
- For restart format details: `engine/source/output/restart/wrrestp.F` writes what `LECTUR` reads
