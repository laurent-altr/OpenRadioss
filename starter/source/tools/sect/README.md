# Section Cuts (`starter/source/tools/sect/`)

Reads and initialises `/SECT` (section cut) definitions used for force and energy output through a plane.

## Key Files

| File | Role |
|------|------|
| `prelecsec.F` | Pre-read: count and allocate section definitions |
| `hm_read_sect.F` | HM binary reader for `/SECT` definitions |
| `lecsec4bolt.F` | Read section definitions for bolt pretension (`/PRET` bolt cross-section) |
| `prelecsec4bolt.F` | Pre-read bolt section definitions |

## What is a Section?

A section cut (`/SECT`) defines a plane through the model. At each output step, the engine:
1. Clips all element faces against the section plane (using the polygon clipping in `common_source/tools/clipping/`)
2. Integrates forces across the cut plane
3. Outputs the resultant force/moment vector in the section normal direction

This is equivalent to a virtual "force sensor" at any cross-section of the model.

## Bolt Sections

`lecsec4bolt.F` handles the special case where a `/SECT` is used to define the cross-section of a bolt shank for the pretension load (`/PRET`). The bolt preload is applied as an equivalent force distributed over the bolt cross-section.

## Related Documentation

- `engine/source/tools/README.md` — engine section cut computation (`cutmain.F`)
- `starter/source/tools/README.md` — parent tools directory
- `common_source/tools/clipping/README.md` — polygon clipping used for section cutting
