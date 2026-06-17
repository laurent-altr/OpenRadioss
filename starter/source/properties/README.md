# Starter Properties Subsystem

This subsystem reads all `/PROP/TYPE<N>` section/property keyword definitions and validates the geometric and formulation parameters.

## Directory Structure

```
properties/
├── beam/                  — Beam section property readers
├── composite_options/     — Composite laminate (ply stack) property readers
├── injector/              — Injector property for airbag gas generators
├── rivet/                 — Rivet / spot-weld connector property readers
├── shell/                 — Shell property readers (TYPE1, TYPE2, TYPE11, TYPE43, …)
├── solid/                 — Solid property readers (TYPE6, TYPE7, TYPE8, TYPE14, …)
├── sph/                   — SPH property readers (TYPE10)
├── hm_preread_properties.F — Pre-read property keyword list (HM format)
├── hm_read_prop_generic.F  — Generic HM-format property reader
└── hm_read_properties.F    — Top-level HM property reader dispatcher
```

## Property Reading

For each property type, the starter:
1. Reads the keyword parameters into the `PM`/`IPM` property arrays
2. Validates ranges and consistency (e.g. thickness > 0, integration scheme valid)
3. Applies defaults for optional parameters
4. Computes derived geometric quantities (e.g. beam cross-section area from dimensions)

## Shell Properties

Shell properties (TYPE1, TYPE2, TYPE10, TYPE11, TYPE43) include:
- Thickness and thickness integration scheme (# through-thickness integration points)
- Hourglass control method and coefficient (QEPH, HEPH, Flanagan-Belytschko)
- Inplane integration type (full, reduced, selective)
- Membrane / bending stiffness split flags

## Composite Properties (`composite_options/`)

For TYPE43 and multi-ply composites, the ply stack table is built:
- Ply orientation angles (relative to element reference frame)
- Ply thicknesses (can vary per ply)
- Ply material IDs
- Reference surface (mid-plane, top, bottom)

## Injector Properties (`injector/`)

Reads the gas injector geometry for airbag models:
- Injector orifice area and direction
- Mass flow rate curve reference
- Temperature and composition of injected gas

## Related Documentation

- `engine/source/properties/README.md` — how property data is used at runtime
- `engine/source/elements/README.md` — elements that consume property data
- `starter/source/materials/README.md` — material data paired with properties
