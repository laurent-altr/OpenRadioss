# CFG Unit System (`reader/source/cfgkernel/MUNITS/`)

Unit-of-measure management for the CFG reader: named unit definitions, dimensional analysis, and unit conversion operations.

## Key Files

| File | Role |
|------|------|
| `mu_dimension.cpp` / `.h` | Physical dimension representation (mass, length, time, …) |
| `mu_named_unit.cpp` / `.h` | Named unit definitions (kg, mm, ms, N, MPa, …) |
| `mu_operations.cpp` | Unit arithmetic: multiply, divide, power |

## Related Documentation

- `reader/source/cfgkernel/README.md` — parent directory
- `reader/source/cfgkernel/HCDI/README.md` — HCDI uses units for field value conversion
