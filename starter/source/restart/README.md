# Starter Restart Subsystem

This subsystem writes the binary restart file (`_0001.rad`) that is the primary output of the starter and the primary input of the engine. It also contains the domain decomposition split utilities.

## Directory Structure

```
restart/
└── ddsplit/       — Domain-decomposed restart splitting utilities
```

## What the Restart File Contains

The `_0001.rad` binary file encodes the complete initialised model:

| Data category | Description |
|---------------|-------------|
| Node data | Coordinates, masses, initial velocities, constraint flags |
| Element data | Connectivity, element type flags, OFF status, history variable sizes |
| Material data | Real (PM) and integer (IPM) property tables for all materials |
| Property data | Section geometry, formulation flags |
| Interface data | Contact pair lists, gap parameters, interface buffer data |
| Load data | Load curve tables, force/pressure definitions |
| Group data | Node, element, and surface group member lists |
| ALE data | ALE mesh topology, multi-material assignments |
| Domain decomp. | Rank assignment, ghost node maps, send/receive patterns |
| Initial state | Initial stress/strain fields (from `/INISTA`, `/INIMAP`) |

## Restart Write Sequence

The starter writes the restart in a sequential block format:

```
write header (version, model size)
for each data category:
    write block tag
    write array data (via write_array.F from common_source/comm/)
write checksum
```

Each block tag allows the engine reader (`engine/source/input/`) to identify and skip unknown blocks for forward compatibility.

## Domain-Decomposed Split (`ddsplit/`)

For parallel runs, the restart file is split into `N` per-rank files (`_0001_<rank>.rad`) after the serial write. The `ddsplit/` routines perform this split, reading the global restart and writing only the data relevant to each rank (owned elements/nodes + ghost layer).

This split step is the last operation performed by the starter before it exits.

## Related Documentation

- `engine/source/input/README.md` — engine reads this file at startup
- `starter/source/spmd/README.md` — domain decomposition that determines the split
- `common_source/comm/README.md` — `write_array.F` used for binary writing
