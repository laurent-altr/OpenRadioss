# CFG Kernel (`reader/source/cfgkernel/`)

Expression parser and keyword evaluation engine for the HyperWorks CFG format: evaluates mathematical expressions, handles variable substitution, and dispatches keyword blocks to per-entity readers.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `KERNEL/` | Core parser: `cfg_kernel.cpp/h` (main parser), expression operators (`dr_binary_operator`, `dr_unary_operator`, `dr_expression`), `general_memory.c/h`, per-format subdirs (`radioss/`, `ls-dyna/`) |
| `General/` | General utilities: `convert.h`, `convertRuleMap.h`, `convertRuleParser.h` — rule-based conversion from one solver format to another |
| `HCDI/` | HyperCrash Data Interface: high-level API between CFG kernel and GUI tools |
| `KERNEL_BASE/` | Base kernel types: abstract interfaces for the expression/keyword system |
| `MESSAGE/` | Message handling: warnings, errors, info messages during parsing |
| `MUNITS/` | Unit system handling: unit conversion between SI, mm/N/s, CGS, etc. |
| `PARSER/` | Low-level tokeniser and grammar for CFG keyword blocks |
| `UTILS/` | General utility functions |

## Architecture

The CFG kernel is a general-purpose expression-evaluating keyword parser shared across multiple Altair products. The `KERNEL/radioss/` subdirectory contains the Radioss-specific reader rules (material laws, element types, boundary conditions) as table-driven converters calling the SDI API.

## Related Documentation

- `reader/source/cfgio/README.md` — CFG I/O layer
- `reader/source/sdi/README.md` — SDI model data interface
