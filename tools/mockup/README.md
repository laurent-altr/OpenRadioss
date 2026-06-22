# Mockup Harnesses (`tools/mockup/`)

Minimal self-contained programs extracted from OpenRadioss for isolated
performance testing and algorithm development.  Each mockup compiles and
runs without the full solver — useful for benchmarking, profiling, and
compiler-flag experiments on individual subsystems.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `contact_sandbox/` | Standalone TYPE7 broad-phase collision detection (voxel-based candidate search) |
| `element_sandbox/` | Standalone shell element computation loop (LAW1/LAW2 material, velocity integration) |

## Related Documentation

- `tools/mockup/contact_sandbox/README.md` — contact mockup build and run instructions
- `tools/mockup/element_sandbox/Readme.md` — element mockup build and run instructions
- `tools/README.md` — parent tools directory
