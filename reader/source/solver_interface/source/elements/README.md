# Solver Interface — Element Readers (`reader/source/solver_interface/source/elements/`)

Fortran-callable wrappers that extract element connectivity and property
data from the SDI model into Fortran arrays for the starter kernel.

## Key Files

| File | Role |
|------|------|
| `cpp_shell_read.cpp` | 4-node shell (QUAD4): connectivity `IXC`, thickness `THK`, integration angle |
| `cpp_sh3n_read.cpp` | 3-node shell (TRIA3) reader |
| `cpp_shel16_read.cpp` | 16-node thick-shell reader |
| `cpp_brick_read.cpp` | 8-node hexahedral solid (HEXA8) reader |
| `cpp_brick20_read.cpp` | 20-node hexahedral solid reader |
| `cpp_tetra4_read.cpp` | 4-node tetrahedral solid reader |
| `cpp_tetra10_read.cpp` | 10-node tetrahedral solid reader |
| `cpp_penta6_read.cpp` | 6-node pentahedral (wedge) solid reader |
| `cpp_quad_read.cpp` | Quadrilateral element reader |
| `cpp_tria_read.cpp` | Triangular element reader |
| `cpp_beam_read.cpp` | Beam element reader |
| `cpp_spring_read.cpp` | Spring / scalar element reader |
| `cpp_truss_read.cpp` | Truss element reader |
| `cpp_rivet_read.cpp` | Rivet element reader |
| `cpp_sphcel_read.cpp` | SPH particle reader |
| `cpp_xelem_read.cpp` / `cpp_xelem_preread.cpp` | XFEM enriched element reader and pre-read pass |
| `cpp_elem_count.cpp` | Element count per type/part |

## Related Documentation

- `reader/source/solver_interface/source/README.md` — parent directory
