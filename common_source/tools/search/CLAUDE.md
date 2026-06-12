# tools/search/

## Purpose
Binary (dichotomic) search variants for sorted arrays. Returns the index `i` such that `array(i) <= value < array(i+1)`. Covers ascending/descending order and both real and integer element types.

## Files

| File | Description |
|------|-------------|
| `dichotomic_search_r_asc.F` | Subroutine `dichotomic_search_r_asc` — binary search for a real value in an ascending-sorted real array |
| `dichotomic_search_r_desc.F` | Subroutine `dichotomic_search_r_desc` — binary search for a real value in a descending-sorted real array |
| `dichotomic_search_i_asc.F` | Subroutine `dichotomic_search_i_asc` — binary search for an integer value in an ascending-sorted integer array |
| `dichotomic_search_i_desc.F` | Subroutine `dichotomic_search_i_desc` — binary search for an integer value in a descending-sorted integer array |

## Notes
- All four variants have the same interface shape; select based on array element type and sort order.

## Dependencies
- Uses: `modules/precision_mod.F90` (real variant)
- Used by: EOS table interpolation lookups, time history interval searches, and any routine doing interval location in sorted data
