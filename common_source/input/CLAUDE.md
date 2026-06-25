# input/

## Purpose
Input text parsing utility shared by starter and engine.

## Files

| File | Description |
|------|-------------|
| `nvar.F` | `INTEGER FUNCTION NVAR(TEXT)` — counts the number of whitespace-delimited tokens in a text string; used during keyword-based input parsing |

## Dependencies
- Used by: input readers in the starter that need to know how many values are on a line before allocating buffers
