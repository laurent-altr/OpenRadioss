# DYNA2RAD Internal Utilities (`reader/source/dyna2rad/dyna2rad/_private/`)

Internal implementation files for the DYNA2RAD converter: base conversion class, rule map/parser, and utility types.

## Key Files

| File | Role |
|------|------|
| `convert.h` / `convert.cxx` | Base converter class: manages the conversion pipeline |
| `convertRuleMap.h` / `convertRuleMap.cxx` | Rule map: maps LS-DYNA keyword patterns to converter functions |
| `convertRuleParser.h` / `convertRuleParser.cxx` | Rule parser: parses conversion rule definitions from config |
| `convertutilsbase.h` / `convertutilsbase.cxx` | Base utility functions shared by all converters |
| `typedef.h` | Common type aliases for the converter |

## Related Documentation

- `reader/source/dyna2rad/dyna2rad/README.md` — parent converter directory
