# SDI Converter (`reader/source/sdi/converter/`)

Public API for the SDI format-conversion layer: rules, rule maps, and the
rule-parser that translate between different entity vocabularies (e.g.
LS-DYNA keyword names → Radioss keyword names) inside the SDI model.

## Key Files

| File | Role |
|------|------|
| `convert.h` | `Convert` class: drives a conversion pass; `ClientInfo` carries per-client state |
| `convertRuleMap.h` | `convertRule` (one mapping entry) + `convertRuleMap` (ordered collection of rules) |
| `convertRuleParser.h` | `ConvertRuleParser`: parses rule definition files (extends `MvParserBase_t`) |
| `convertutilsbase.h` | Utility base types shared across conversion rules |
| `typedef.h` | Common `typedef` aliases used throughout the converter |

## Related Documentation

- `reader/source/sdi/converter/_private/README.md` — implementation files
- `reader/source/sdi/README.md` — parent SDI library
- `reader/source/dyna2rad/dyna2rad/README.md` — consumer of this layer
