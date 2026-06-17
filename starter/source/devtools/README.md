# Starter Developer Tools

This directory contains developer utilities for the starter — tools used during development and integration testing rather than in production runs.

## Directory Structure

```
devtools/
└── hm_reader/    — HyperMesh binary format reader
```

## HyperMesh Reader (`hm_reader/`)

The HyperMesh (HM) reader provides an alternative input path: instead of parsing a text keyword deck, the starter can read geometry and model data directly from HyperMesh's binary format. This is used in tightly integrated Altair workflow pipelines (HyperWorks → OpenRadioss).

The HM reader is an alternative to the standard keyword file (`_0000.rad`) — the user passes the HM binary file path instead of the keyword file.

Many of the `hm_read_*.F` files scattered across the starter subsystem directories (e.g. `elements/reader/hm_read_node.F`, `properties/hm_read_properties.F`) are driven by this HM reader subsystem.

## Usage

The HM reader is activated by a command-line flag:

```bash
starter_linux64_gf -input model.hm
```

It is otherwise transparent — the same model data structures are produced regardless of whether input came from text keywords or HM binary.

## Related Documentation

- `starter/source/README.md` — overall starter pipeline
- `starter/source/elements/README.md` — element HM readers (`reader/` subdirectory)
