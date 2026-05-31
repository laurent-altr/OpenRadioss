# qa/

## Purpose
Quality assurance utilities: a diagnostic output module used throughout the solver, and process-management helpers for zombie detection (QA runs only).

## Files

| File | Description |
|------|-------------|
| `qa_out_mod.F` | Module `QA_OUT_MOD` — provides debug-print and diagnostic output subroutines used throughout starter and engine for selective QA output (controlled by `/QA` keywords) |
| `kill_zombi.F` | Subroutines `get_env_variable_zombi`, `get_process_pid_zombi`, `write_process_pid_zombi` — QA-only utilities for process PID tracking and zombie process detection; not part of the main solver execution path |

## Key Modules Exported
- **`QA_OUT_MOD`** (`qa_out_mod.F`) — diagnostic output interface

## Dependencies
- Used by: wide range of starter and engine routines that conditionally emit QA diagnostics
