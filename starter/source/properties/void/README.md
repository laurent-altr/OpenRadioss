# Void Property (`starter/source/properties/void/`)

Reads void (null) element properties: elements with zero stiffness used as placeholders for element activation or ALE void regions.

## Key Files

| File | Role |
|------|------|
| `hm_read_prop28.F` | Read `/PROP/TYPE28` void property |

## Description

Void elements occupy mesh space without contributing stiffness or mass. They are used in two contexts:
1. **Element activation** (`/ACTIV`): void elements become active (gain material properties) at a user-specified time or trigger, modelling casting solidification, progressive laminate stacking, or explosive fill
2. **ALE void regions**: ALE/Euler cells initialised as void fill with fluid as the simulation progresses

`hm_read_prop28.F` reads the minimal property data needed (density for mass when activated, activation material reference).

## Related Documentation

- `starter/source/properties/README.md` — parent directory
- `starter/source/tools/activ/README.md` — element activation
