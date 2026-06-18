# LAW158 — User-Defined Shell Law (`engine/source/materials/mat/mat158/`)

Interface for user-defined shell material laws: provides the call-out
hook and state-variable buffer for custom Fortran constitutive routines
plugged in via the user-material interface.

## Key Files

| File | Role |
|------|------|
| `sigeps158c.F` | Shell stress-update wrapper: calls user-supplied subroutine |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
