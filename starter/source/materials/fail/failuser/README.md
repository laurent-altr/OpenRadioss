# User-Defined Failure (`starter/source/materials/fail/failuser/`)

Starter reader for /FAIL/USER: user-defined failure criterion via external Fortran subroutine.

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_user.F` | Parse /FAIL/USER card parameters (user subroutine ID, parameter array) |

## Description

`/FAIL/USER` allows attaching a custom failure criterion written in Fortran. The starter reads the subroutine ID and parameter array and writes them to the restart file. At runtime the engine calls the user subroutine at each material point to evaluate the failure indicator.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/failuser/README.md` — runtime user failure hook
