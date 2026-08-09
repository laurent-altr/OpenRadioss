# Common Tools (common_source/tools)

This directory provides general-purpose algorithmic utilities shared between the starter and engine. These are pure computational routines with no physics content.

## Directory Structure

```
tools/
├── clipping/      — Geometric clipping algorithms
├── container/     — Generic container data structures (lists, queues)
├── graphs/        — Graph algorithms (connectivity, adjacency)
├── input_output/  — Low-level text and binary I/O helpers
├── interpolation/ — 1D and multi-dimensional interpolation
├── math/          — Mathematical utilities (statistics, random numbers)
├── memory/        — Memory allocation wrappers (`MY_ALLOC`)
├── search/        — Search algorithms (binary search, hash tables)
├── set_operation/ — Set operations (union, intersection, difference)
└── sort/          — Sorting algorithms (quicksort, merge sort, radix)
```

## Memory Allocation (`memory/`)

Contains `MY_ALLOC` — the standard allocation routine used throughout OpenRadioss. Always use this instead of bare Fortran `ALLOCATE`:

```fortran
use MY_ALLOC_MOD, only : my_alloc
call my_alloc(array, size, 'array_name in subroutine_name')
```

`MY_ALLOC` allocates, checks status, and calls `ARRET` on failure with a meaningful message.

## Sorting (`sort/`)

Multiple sort algorithms for different use cases:
- Integer sorting (element/node IDs — very common in mesh operations)
- Floating-point sorting (for geometry operations)
- Radix sort for large arrays of bounded integers (O(n) vs O(n log n))

## Search (`search/`)

- Binary search on sorted arrays
- Hash table implementation for O(1) average node/element look-up
- Spatial search (bounding-box queries for contact sorting)

## Interpolation (`interpolation/`)

1D and multi-dimensional table interpolation used by material models, function curves, and field mapping. More focused utilities are in `engine/source/tools/curve/`; this directory provides the fundamental primitives.

## Graph Algorithms (`graphs/`)

Connectivity analysis on the mesh graph:
- Connected component labelling (find isolated mesh regions)
- Adjacency list construction (node-to-element neighbourhood)
- Colour assignment (for parallel OpenMP assembly without race conditions)

## Clipping (`clipping/`)

Sutherland-Hodgman polygon clipping. Used for:
- ALE interface tracking (clip fluid cells against structural boundaries)
- Output section cuts (clip element faces against the cut plane)

## Related Documentation

- `engine/source/tools/README.md` — engine-specific tools (function curves, sensors, etc.)
- `common_source/README.md` — overview of all common_source components
- `.github/copilot-instructions.md` — `MY_ALLOC` usage rules
