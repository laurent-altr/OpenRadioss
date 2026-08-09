# H3D C++ API (`engine/source/output/h3d/h3d_build_cpp/`)

C++ wrapper layer over the Altair H3D binary format library: creates data type descriptors and mesh topology required by the H3D writer.

## Key Files

| File | Role |
|------|------|
| `c_h3d_close_file.cpp` | Close H3D file and flush all pending data |
| `c_h3d_create_1d_scalar_datatype.cpp` | Create H3D datatype for 1D element scalar result |
| `c_h3d_create_1d_tensor_datatype.cpp` | Create H3D datatype for 1D element tensor result |
| `c_h3d_create_1d_torsor_datatype.cpp` | Create H3D datatype for force/moment torsor result |
| `c_h3d_create_1d_vector_datatype.cpp` | Create H3D datatype for 1D element vector result |
| `c_h3d_create_beams.cpp` | Register beam element topology with H3D |
| `c_h3d_create_components.cpp` | Register component (part) grouping with H3D |
| `c_h3d_create_displacement_datatype.cpp` | Create H3D datatype for nodal displacement |
| `c_h3d_create_nodal_scalar_datatype.cpp` | Create H3D datatype for nodal scalar result |
| `c_h3d_create_nodal_tensor_datatype.cpp` | Create H3D datatype for nodal tensor result |

## Description

These C++ functions are called from Fortran via ISO C binding to register the mesh topology and result data types with the H3D library before any result data is written. The H3D library (external Altair library) manages the binary file format; these wrappers provide a Fortran-callable interface.

## Related Documentation

- `engine/source/output/h3d/README.md` — parent H3D directory
- `engine/source/output/h3d/h3d_build_fortran/README.md` — Fortran result data builders
