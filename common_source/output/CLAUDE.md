# output/

## Purpose
Top-level binary output file writing utility; provides a Fortran-callable wrapper for writing formatted text lines through a C file descriptor.

## Files

| File | Description |
|------|-------------|
| `write_out_file.F90` | Subroutine `write_out_file(fd, line, len1)` — writes one formatted text line to an output file given a C-style file descriptor; enables C code to trigger Fortran-format writes |

## Dependencies
- Used by: output routines that need to write formatted lines to `.out` report files
