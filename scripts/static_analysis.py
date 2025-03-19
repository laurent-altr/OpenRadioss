import re
import os
from collections import defaultdict
import logging
from pathlib import Path



def extract_function_scopes(lines):
    """Extracts function definitions (subroutines) from DISubprogram."""
    scope_map = {}  # Map of scope ID → function name
    logging.info("\n🔍 Extracting function scopes from DISubprogram...")
    for line in lines:
        match = re.match(r'!(\d+) = distinct !DISubprogram\(name: "(\w+)", .* scope: !(\d+),', line)
        if match:
            scope_id, func_name, parent_scope = match.groups()
            scope_map[f"!{scope_id}"] = func_name
            logging.info(f"  ✅ Found function: {func_name} (Scope ID: !{scope_id})")

    return scope_map

def extract_type_definitions(lines):
    """Extracts type definitions from DIBasicType and DIDerivedType."""
    type_definitions = {}

    logging.info("\n🔍 Extracting type definitions...")

    for line in lines:
        line = line.strip()
        # Match basic types (DIBasicType)
#       match_basic = re.match(r'!(\d+) = !DIBasicType\(name: "([^"]+)",', line)
#        match_basic = re.match(r'!(\d+) = !DIBasicType\(name: "([^"]+)",.*\)', line.strip())
        match_basic = re.match(r'!(\d+) = !DIBasicType\(name: "([^"]+)", size: (\d+), encoding: (\w+)\)', line.strip())
        
        if match_basic:
            type_id, type_name, size, encoding = match_basic.groups()  # Capture all four values
            type_definitions[f"!{type_id}"] = {"name": type_name, "size": size, "encoding": encoding}  # Store all values
            logging.info(f"  ✅ Found basic type: !{type_id} → {type_name} (Size: {size}, Encoding: {encoding})")

    for line in lines:
        line = line.strip()
        # Match derived types (DIDerivedType, e.g., typedefs)
        match_derived = re.match(r'!(\d+) = !DIDerivedType\(.* name: "([^"]+)", baseType: !(\d+)', line)
        if match_derived:
            type_id, derived_name, base_id = match_derived.groups()
            base_type = type_definitions.get(f"!{base_id}", f"!{base_id}")  # Resolve base type if known
            type_definitions[f"!{type_id}"] = derived_name if derived_name else base_type
            logging.info(f"  ✅ Found derived type: !{type_id} → {derived_name} (base: {base_type})")
    for line in lines:
        line = line.strip()
        match_array = re.match(r'!(\d+) = !DICompositeType\(tag: DW_TAG_array_type, baseType: !(\d+),.*\)', line.strip())
        if match_array:
            type_id, base_type_id = match_array.groups()
            # Resolve base type if it's already parsed
            if f"!{base_type_id}" in type_definitions:
                resolved_base = type_definitions[f"!{base_type_id}"]
            else:
                resolved_base = f"!{base_type_id}"  # Keep unresolved reference if not found yet
        
            type_definitions[f"!{type_id}"] = f"Array of {resolved_base}"  # Store resolved type
            logging.info(f"  ✅ Found array type: !{type_id} → Array of {resolved_base}")

    for line in lines:
        line = line.strip()
        match_array = re.match(r'!(\d+) = !DICompositeType\(tag: DW_TAG_array_type, baseType: !(\d+),', line)
        if match_array:
            type_id, base_id = match_array.groups()
            base_type = type_definitions.get(f"!{base_id}", f"!{base_id}")  # Resolve base type if known
            type_definitions[f"!{type_id}"] = f"Array of {base_type}"
            logging.info(f"  ✅ Found array type: !{type_id} → Array of {base_type}")


    return type_definitions


def extract_variable_mappings(lines, scope_map, type_definitions):
    """Extracts variable names and types from DILocalVariable and groups them by subroutine scope."""
    subroutine_data = defaultdict(lambda: {"register_map": {}, "type_info": {}, "calls": []})

    logging.info("\n🔍 Extracting variable mappings from DILocalVariable...")

    # Regex for function arguments (those containing 'arg:')
    arg_pattern = re.compile(r'!(\d+) = !DILocalVariable\(name: "([^"]+)", arg: \d+, scope: !(\d+), file: !\d+, line: \d+, type: !(\d+)\)')
    
    # Regex for local variables (without 'arg:')
    local_var_pattern = re.compile(r'!(\d+) = !DILocalVariable\(name: "([^"]+)", scope: !(\d+), file: !\d+, line: \d+, type: !(\d+)\)')

    for line in lines:
        line = line.strip()
        
        # First, check if it matches an argument
        match = arg_pattern.match(line)
        if not match:
            # If not, check if it's a local variable
            match = local_var_pattern.match(line)

        if match:
            metadata_id, var_name, scope_id, type_id = match.groups()
            scope_key = f"!{scope_id}"

            if scope_key in scope_map:
                subroutine = scope_map[scope_key]
                subroutine_data[subroutine]["register_map"][f"!{metadata_id}"] = var_name

#               subroutine_data[subroutine]["type_info"][var_name] = type_definitions.get(f"!{type_id}", f"!{type_id}")
                type_found = type_definitions.get(f"!{type_id}", f"!{type_id}")
                if type_found:
                    subroutine_data[subroutine]["type_info"][var_name] = type_found
                else:
                    logging.warning(f"  ⚠️ Warning: Type ID {type_id} not found for variable {var_name} in subroutine {subroutine}")
                    subroutine_data[subroutine]["type_info"][var_name] = f"!{type_id}"
                logging.info(f"  ✅ {subroutine}: Stored Variable {var_name} (Type: !{type_id})")
            else:
                logging.warning(f"  ⚠️ Warning: Scope {scope_id} not found in scope_map for line: {line}")

    return subroutine_data


def extract_dbg_declare_mappings(lines, subroutine_data):
    """Extracts dbg_declare mappings for ptr %X → variable name and associates them with subroutines."""
    logging.info("🔍 Extracting dbg_declare mappings (ptr %X → variable name)...")
    
    for line in lines:
        if "dbg_declare" in line:
            match = re.match(r'#dbg_declare\(ptr (%\d+), !(\d+),', line.strip())
            if match:
                register, metadata_id = match.groups()
                meta_key = f"!{metadata_id}"

                for subroutine, data in subroutine_data.items():
                    if meta_key in data["register_map"]:
                        data["register_map"][f"ptr {register}"] = data["register_map"][meta_key]
                        logging.info(f"  ✅ {subroutine}: Mapped ptr %{register} → {data['register_map'][meta_key]}")
                        break
                else:
                    logging.warning(f"  ⚠️ Warning: Metadata ID {meta_key} not found in any subroutine for line: {line.strip()}")

def extract_function_calls(lines, subroutine_data, dbg_map):
    """Extracts function calls and resolves register arguments using debug locations and store instructions."""
    logging.info("🔍 Extracting function calls...")

    unresolved_ptrs = {}  # Stores unresolved ptr %X and their associated call site (by Fortran line)
    store_mappings = {}  # Maps ptr %X → (type, value) from store instructions

    # **PASS 1**: Extract function calls, keeping unresolved ptr %X
    for line_num, line in enumerate(lines, start=1):
        line = line.strip()
        
        # Match function calls
        match = re.match(r'call void @(\w+)\((.*)\), !dbg !(\d+)', line)
        if match:
            callee, args, dbg_id = match.groups()
            dbg_key = f"!{dbg_id}"
            
            if dbg_key in dbg_map:
                # Find which subroutine this call belongs to
                subroutine, fortran_line = dbg_map[dbg_key]  # Unpack the tuple
                
                call_info = {
                    "caller": subroutine,
                    "callee": callee,
                    "args": [arg.strip() for arg in args.split(',') if arg.strip()],
                    "original_code_line": fortran_line
                }
                
                resolved_args = []
                for arg in call_info["args"]:
                    if arg.startswith("ptr %"):  # Unresolved argument
                        unresolved_ptrs[arg] = fortran_line  # Store for later resolution
                        resolved_args.append(arg)  # Keep unresolved for now
                    else:
                        resolved_args.append(arg)  # Constants or known variables

                call_info["args"] = resolved_args
                subroutine_data[subroutine]["calls"].append(call_info)

                logging.info(f"  ✅ {subroutine}: Call to {callee} at line {call_info['original_code_line']}")
            else:
                logging.warning(f"  ⚠️ Warning: No scope found for function call: {line}")

    # **PASS 2**: Extract `store` instructions mapping ptr %X → value/type
    logging.info("🔍 Resolving stored values for unresolved pointers...")
    for line in lines:
        line = line.strip()
        
        # Match store instructions like:
        # store i32 1, ptr %55, align 4, !dbg !2210
        store_match = re.match(r'store (\w+\d+) (\S+), ptr (%\d+), .* !dbg !(\d+)', line)
        if store_match:
            var_type, value, ptr, dbg_id = store_match.groups()
            dbg_key = f"!{dbg_id}"
            
            if dbg_key in dbg_map:
                fortran_line = dbg_map[dbg_key][1]  # Extract Fortran line number

                if f"ptr {ptr}" in unresolved_ptrs and unresolved_ptrs[f"ptr {ptr}"] == fortran_line:
                    # Store type and value resolution
#                    store_mappings[f"ptr {ptr}"] = f"{var_type} {value}"
                    store_mappings[f"ptr {ptr}"] = value  # Store only the actual constant

                    logging.info(f"  ✅ Resolved {ptr}: Type {var_type}, Value {value} (Fortran line {fortran_line})")

    # **PASS 3**: Apply the resolutions to function call arguments
    logging.info("🔍 Applying resolved values to function call arguments...")
    for subroutine, data in subroutine_data.items():
        for call in data["calls"]:
            resolved_args = []
            resolved_types = []  # Track types
    
            for arg in call["args"]:
                # **Case 1: Variables (resolve from register_map and type_info)**
                if arg in subroutine_data[subroutine]["register_map"]:
                    var_name = subroutine_data[subroutine]["register_map"][arg]
                    resolved_args.append(var_name)
                    resolved_types.append(subroutine_data[subroutine]["type_info"].get(var_name, "unknown"))
    
                # **Case 2: Constants (resolve from store_mappings)**
                elif arg in store_mappings:
                    resolved_args.append(store_mappings[arg])
                    resolved_types.append("integer" if store_mappings[arg].isdigit() else "unknown")
    
                # **Case 3: Keep unresolved**
                else:
                    resolved_args.append(arg)
                    resolved_types.append("unknown")
    
            # Update call arguments and add types
            call["args"] = resolved_args
            call["argsType"] = resolved_types
    
    logging.info("✅ Function call argument resolution complete.")



def extract_dbg_locations(lines, scope_map):
    """Extracts DILocation (debug info) to map function calls to their subroutine and Fortran line number."""
    dbg_map = {}  # Map of !dbg ID → {subroutine, line}
    
    logging.info("\n🔍 Extracting debug locations (DILocation)...")
    for line in lines:
        line = line.strip()
        match = re.match(r'!(\d+) = !DILocation\(line: (\d+), column: \d+, scope: !(\d+)', line)
        if match:
            dbg_id, fortran_line, scope_id = match.groups()
            scope_key = f"!{scope_id}"
            if scope_key in scope_map:
                subroutine = scope_map[scope_key]
                dbg_map[f"!{dbg_id}"] = (subroutine, fortran_line)  # Store as a tuple
                logging.info(f"  ✅ Mapped !dbg {dbg_id} to subroutine {subroutine}, Fortran line {fortran_line}")
            else:
                logging.warning(f"  ⚠️ Warning: Scope {scope_id} not found for debug location: {line.strip()}")
        else:
            match = re.match(r'!(\d+) = !DILocation\(line: (\d+), scope: !(\d+)', line) 
            if match:
                dbg_id, fortran_line, scope_id = match.groups()
                scope_key = f"!{scope_id}"
                if scope_key in scope_map:
                    subroutine = scope_map[scope_key]
                    dbg_map[f"!{dbg_id}"] = (subroutine, fortran_line)  # Store as a tuple
                    logging.info(f"  ✅ Mapped !dbg {dbg_id} to subroutine {subroutine}, Fortran line {fortran_line}")
                else:
                    logging.warning(f"  ⚠️ Warning: Scope {scope_id} not found for debug location: {line.strip()}")
    
    return dbg_map

def parse_llvm_ir(file_path):
    """Coordinates the parsing of an LLVM IR file."""
    logging.info(f"🔍 Parsing {file_path}...")

    with open(file_path, "r") as f:
        lines = f.readlines()

    # Step 1: Extract function scopes (DISubprogram)
    scope_map = extract_function_scopes(lines)


    # Step 2: Extract debug location mappings (!dbg → subroutine and Fortran line)
    dbg_map = extract_dbg_locations(lines, scope_map)
    type_definitions = extract_type_definitions(lines)
    # Step 3: Extract variable mappings (DILocalVariable)
    subroutine_data = extract_variable_mappings(lines, scope_map,type_definitions)

    # Step 4: Extract ptr %X to variable name mappings from dbg_declare
    extract_dbg_declare_mappings(lines, subroutine_data)

    # Step 5: Extract function calls and resolve arguments
    extract_function_calls(lines, subroutine_data, dbg_map)

    return subroutine_data


def find_object_files(directory):
    """Recursively find all .o files in the given directory."""
    return list(Path(directory).rglob("*.F.o"))+list(Path(directory).rglob("*.F90.o"))

if __name__ == "__main__":
    logging.basicConfig(level=logging.WARNING, format="%(levelname)s: %(message)s")

    # Set the root directory where .o files are stored
    root_dir = "../engine/cbuild_engine_linux64_flang_db/"

    logging.info(f"🔍 Searching for .o files in {root_dir}...")
    object_files = find_object_files(root_dir)

    if not object_files:
        logging.error("❌ No .o files found.")
        exit(1)

    all_subroutines = {}

    logging.info(f"📂 Found {len(object_files)} .o files. Parsing them now...")

    for obj_file in object_files:
        try:
            logging.info(f"📜 Parsing {obj_file} ...")
            subroutine_data = parse_llvm_ir(str(obj_file))  # Ensure it's a string path
            all_subroutines.update(subroutine_data)  # Merge subroutine data across files
        except UnicodeDecodeError as e:
            logging.error(f"❌ UnicodeDecodeError in file: {obj_file} → {e}")


#   for obj_file in object_files:
#       logging.info(f"📜 Parsing {obj_file} ...")
#       subroutine_data = parse_llvm_ir(str(obj_file))  # Ensure it's a string path
#       all_subroutines.update(subroutine_data)  # Merge subroutine data across files

    logging.info(f"✅ Parsing complete. Collected {len(all_subroutines)} subroutines.")

    # Debug output (optional)
    for subroutine, data in all_subroutines.items():
        logging.info(f"🔹 Subroutine: {subroutine}")
        logging.info(f"    📌 Calls: {[call['callee'] for call in data['calls']]}")

