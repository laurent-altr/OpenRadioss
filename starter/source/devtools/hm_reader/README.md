# HM Reader API (`starter/source/devtools/hm_reader/`)

Fortran wrapper API over the HyperMesh (CFG) reader: provides all `hm_get_*`, `hm_option_*`, and utility functions used by the starter's card readers to extract values from the parsed input deck.

## Key Files

| File | Role |
|------|------|
| `hm_option_read.F` | Read and parse a single option block from the deck |
| `hm_option_read_key.F` | Read option by keyword |
| `hm_option_start.F` | Initialise option read cursor |
| `hm_option_next.F` | Advance to next option |
| `hm_option_count.F` | Count occurrences of an option keyword |
| `hm_option_is_encrypted.F` | Check if option block is encrypted |
| `hm_select_option_by_name.F` | Select option matching a name |
| `hm_get_intv.F` | Get integer scalar value from current option |
| `hm_get_floatv.F` | Get floating-point scalar value |
| `hm_get_floatv_dim.F` | Get float with dimensional unit conversion |
| `hm_get_floatv_without_uid.F` | Get float without unit ID |
| `hm_get_boolv.F` | Get boolean value |
| `hm_get_string.F` | Get string value |
| `hm_get_string_index.F` | Get string by column index |
| `hm_get_float_array.F` | Get 1D float array from option |
| `hm_get_float_array_index.F` | Get float array element by index |
| `hm_get_float_array_index_dim.F` | Get float array element with unit conversion |
| `hm_get_float_array_2indexes.F` | Get float array element by 2D index |
| `hm_get_int_array_index.F` | Get integer array element |
| `hm_get_int_array_2indexes.F` | Get integer array element by 2D index |
| `hm_get_max_id.F` | Get maximum entity ID in current model |
| `hm_get_current_option.F` | Return current option keyword |
| `hm_elem_count.F` | Count elements of a given type |
| `hm_entity_reference_number.F` | Resolve entity ID reference |
| `hm_group_is_used.F` | Check if a group is referenced |
| `hm_messages.F` | Issue warning/error messages from reader |
| `hm_debug_print_option.F` | Debug dump of current option data |
| `hm_convert_2d_elements_seatbelt.F` | Convert 2D shell elements to seatbelt segments |
| `hm_convert_fail_tab.F` | Convert legacy tabulated failure format |
| `hm_convert_inter_type19.F` | Convert TYPE19 interface from legacy format |
| `hm_convert_tetra4_to_tetra10.F90` | Upgrade tet4 to tet10 elements |
| `hm_count_2d_element_seatbelt.F` | Count seatbelt-compatible 2D elements |
| `hm_create_rbodies_from_rigid_parts.F90` | Auto-generate /RBODY from rigid parts |
| `hm_evaluate_rbodies_from_rigid_parts.F90` | Compute rigid body properties from rigid parts |
| `hm_rbodies_add_main_node.F90` | Add main node to auto-generated rigid body |
| `hm_set_floatv.F` | Set float value in current option (for defaults) |
| `hm_set_intv.F` | Set integer value in current option |
| `init_random.F` | (here or in computation) random seed init |

## Description

Every card reader in the starter calls these `hm_get_*` routines to extract typed values from the parsed deck. The API abstracts the underlying CFG binary reader and provides unit conversion, range validation, and default-value injection. `hm_option_*` routines iterate over keyword blocks; `hm_get_*` routines extract individual fields.

## Related Documentation

- `starter/source/README.md` — parent starter architecture
- `reader/source/cfgkernel/README.md` — underlying CFG reader kernel
- `reader/source/cfgio/README.md` — CFG binary file I/O
