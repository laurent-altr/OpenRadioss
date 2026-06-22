# Reader I/O (`reader/source/io/`)

Model reader I/O layer: reads various FEA model formats and populates the SDI (Solver Data Interface) object model.

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `model_readers/` | Per-format reader modules: `cfg_reading/` (CFG format), `elements/` (element connectivity), `nodes/` (node coordinates), `model/` (model-level objects), `submodels/` (include files / submodel handling), `includes/` (shared headers), `messages/` (reader diagnostics), `misc/` (miscellaneous utilities) |

## Architecture

The `model_readers` layer provides a unified API for reading multiple input formats (Radioss `.rad`, HyperMesh CFG, LS-DYNA keyword). Each format's sub-reader populates the same SDI entity objects, allowing the GUI tools to work with any supported solver format without format-specific code at the application level.

## Related Documentation

- `reader/source/cfgio/README.md` — CFG binary format
- `reader/source/sdi/README.md` — SDI model data interface
- `reader/source/cfgkernel/README.md` — expression/keyword parser
