# tools/

## Purpose
Generic algorithm and utility library shared by starter and engine. All implementations live in sub-directories; this directory contains only a documentation index file.

## Files

| File | Description |
|------|-------------|
| `tools_function.txt` | Documentation index listing function signatures and descriptions for set operations and sorting algorithms in this library |

## Sub-directories

| Sub-directory | Contents |
|---------------|---------|
| `clipping/` | Polygon clipping using the Weiler-Atherton algorithm |
| `container/` | Hash maps and dynamic integer vectors (C++ std containers with Fortran ISO_C_BINDING wrappers) |
| `graphs/` | Graph path and cycle finding using depth-first search |
| `input_output/` | Binary database read/write for restart files; low-level C I/O routines |
| `interpolation/` | Catmull-Rom spline construction, evaluation, and point projection |
| `math/` | Portable bitwise operations and floating-point precision utilities (C) |
| `memory/` | Array allocation, extension, and shrinking wrappers with error handling |
| `search/` | Binary (dichotomic) search variants for ascending/descending sorted arrays |
| `set_operation/` | Set operations on sorted integer arrays (union, intersect, difference) and set dependency graphs |
| `sort/` | Sorting algorithms: quicksort, insertion sort, radix sort for real, integer, and double arrays |
| `time_step/` | Timestep computation utility |
