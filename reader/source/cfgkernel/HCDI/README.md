# HCDI (HyperWorks CFG Data Interface) (`reader/source/cfgkernel/HCDI/`)

Implements the HCDI abstraction layer: the high-level C++ API through which the Radioss starter queries parsed model entities (elements, nodes, materials, properties) from the CFG reader.

## Key Files

| File | Role |
|------|------|
| `hcdi.h` | Main HCDI API header: entity query functions |
| `hcdi_cond_keyword_list.h` | Conditional keyword list for HCDI queries |
| `hcdi_dimensions.h` | Dimensional unit query interface |
| `hcdi_drawableinf.h` | Drawable entity information interface |
| `hcdi_drawableinf_pre_object.cpp` / `.h` | Pre-object drawable entity implementation |
| `hcdi_mec_pre_object.cpp` / `.h` | Mechanical pre-object entity implementation |
| `hcdi_multicfgkernelmgr.cpp` / `.h` | Manager for multiple simultaneous CFG kernels |

## Description

HCDI (HyperWorks CFG Data Interface) is the abstraction layer between the raw CFG binary parser and the Fortran starter readers. The starter's `hm_get_*` routines call HCDI functions to retrieve field values by name; HCDI translates these into CFG kernel queries and returns typed values with unit conversion.

## Related Documentation

- `reader/source/cfgkernel/README.md` — parent directory
- `starter/source/devtools/hm_reader/README.md` — Fortran wrappers over HCDI
