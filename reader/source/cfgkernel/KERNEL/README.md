# CFG Kernel Core (`reader/source/cfgkernel/KERNEL/`)

Core C++ engine of the HyperWorks CFG format reader: implements the keyword parser, expression evaluator, binary operator tree, and file-format dispatch.

## Key Files

| File | Role |
|------|------|
| `cfg_kernel.cpp` / `cfg_kernel.h` | Main CFG kernel: entry point for parsing a CFG binary or ASCII file |
| `Structure_fileformat_others.cpp` / `.h` | File format handlers for non-standard CFG variants |
| `dr_binary_operator.cpp` / `.h` | Binary operator node in the expression tree |
| `dr_unary_operator.cpp` | Unary operator node in the expression tree |
| `dr_expression.h` | Abstract expression node interface |
| `dr_scalar_operator.h` | Scalar operator node |
| `dr_drawable_operator.h` | Drawable-entity operator node |
| `general_memory.c` / `.h` | Low-level memory allocation wrappers |
| `general_external_functions.h` | External function declarations for CFG kernel |

## Related Documentation

- `reader/source/cfgkernel/README.md` — parent directory
- `reader/source/cfgio/README.md` — CFG binary file I/O
