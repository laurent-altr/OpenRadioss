# Starter Coupling Subsystem

This subsystem reads and validates the external coupling interface definitions (preCICE / CWIPI) and writes the coupling configuration to the restart file.

## Directory Structure

```
coupling/
└── rad2rad/    — OpenRadioss-to-OpenRadioss coupling setup
```

## Role

The starter's coupling subsystem is the **parsing counterpart** to `engine/source/coupling/` (which implements the runtime data exchange). The starter:

1. Reads `/COUPLING` keywords (participant name, mesh, data types)
2. Identifies the coupling surface (node group or element surface)
3. Validates the coupling configuration file reference
4. Writes coupling metadata to the restart file

## Rad2Rad Coupling (`rad2rad/`)

OpenRadioss-to-OpenRadioss coupling allows two separate OpenRadioss instances to exchange data via preCICE. The `rad2rad/` subdirectory handles the special case where both the master and slave are OpenRadioss models:
- Identifies shared coupling surfaces
- Sets up the data exchange map (displacements ↔ forces)
- Configures the synchronisation parameters

## Related Documentation

- `engine/source/coupling/README.md` — full coupling architecture and runtime data exchange
