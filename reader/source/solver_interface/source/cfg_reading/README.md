# Solver Interface — CFG Reading (`reader/source/solver_interface/source/cfg_reading/`)

Fortran-callable wrappers for model lifecycle and option/entity traversal.

## Key Files

| File | Role |
|------|------|
| `cpp_model_open_file.cpp` / `cpp_model_close_file.cpp` | Open/close the CFG model file; populate `g_pModelViewSDI` |
| `GlobalModelSdi.cpp` | `GlobalModelSDISetModel` and `Get_ModelViewSDI` — singleton accessor |
| `PrintOption.cpp` | Debug helper: dump the current entity option to stdout |
| `cpp_option_start.cpp` / `cpp_option_next.cpp` | Begin/advance the entity iteration cursor |
| `cpp_option_read.cpp` | Read the current entity's keyword, ID, title and value type |
| `cpp_option_write.cpp` | Write attribute values back into the SDI model |
| `cpp_option_count.cpp` | Return entity count for a given keyword |
| `cpp_get_intv.cpp` / `cpp_get_floatv.cpp` / `cpp_get_boolv.cpp` / `cpp_get_string.cpp` | Scalar attribute readers |
| `cpp_get_float_array.cpp` | Array attribute reader |
| `cpp_set_intv.cpp` / `cpp_set_floatv.cpp` | Scalar attribute writers |
| `cpp_select_option_by_name.cpp` | Position iteration cursor on a specific keyword |
| `cpp_entity_reference_number.cpp` | Resolve cross-entity reference IDs |
| `cpp_get_max_id.cpp` | Maximum entity ID for a given keyword |
| `cpp_current_option.cpp` | Return keyword of current iteration position |
| `cpp_delete_entity.cpp` | Remove entity from SDI model |
| `cpp_global_entity_sdi_write.cpp` / `cpp_debug_global_entity_sdi_write.cpp` | Write/debug-write global entity attributes |
| `cpp_group_is_used.cpp` | Test whether a group entity is referenced |
| `cpp_add_rbodies_main_node.cpp` | Add rigid-body main node references |
| `cpp_convert_*.cpp` | Post-read conversion helpers (seatbelt 2D, tetra4→tetra10, TYPE19, fail-tab) |
| `cpp_apply_submodel_offsets.cpp` / `cpp_unapply_submodel_offsets.cpp` | Apply/remove submodel ID offsets |
| `cpp_get_submodel_index.cpp` | Look up submodel index from ID |
| `cpp_get_include_files_list.cpp` / `cpp_get_number_of_include_files.cpp` | Include-file list access |
| `cpp_option_is_crypted.cpp` | Test whether entity block is encrypted |

## Related Documentation

- `reader/source/solver_interface/source/README.md` — parent directory
- `reader/source/solver_interface/source/includes/README.md` — shared headers
