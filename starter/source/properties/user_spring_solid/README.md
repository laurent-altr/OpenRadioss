# User Spring-Solid Property (`starter/source/properties/user_spring_solid/`)

Reads properties for user-defined spring-to-solid interface elements combining spring and solid mechanics.

## Key Files

| File | Role |
|------|------|
| `hm_read_prop_user.F` | Read user spring property parameters |
| `hm_read_prop_user4.F` | Read TYPE4 user spring-solid property |

## Description

User spring-solid elements allow user-coded force-displacement laws to be applied between nodes with full 3D coupling. The property card reads user-defined parameters that are passed directly to the user subroutine (`USRSPRING`, `USPRNG4`). These are extension hooks for specialist connector behaviour not covered by built-in spring types, typically used for complex joint models or custom bonding formulations in research applications.

## Related Documentation

- `starter/source/properties/spring/README.md` — standard spring properties
- `starter/source/properties/README.md` — parent directory
