import re
import os
import logging
from pathlib import Path
from collections import defaultdict

# List of function names to ignore
IGNORE_LIST = [
    "FortranAAssign", "FortranAMaxlocDim", "free",
    "FortranAAllocatableSetBounds", "FortranACopyOutAssign",
    "FortranADestroy", "FortranAAbort",
    "FortranAAllocatableInitDerivedForAllocate",
    "FortranACopyInAssign",
    "FortranAPointerSetBounds",
    "FortranAStopStatement"
]


TARGET_METADATA_TYPES = {
    "DIBasicType", "DICommonBlock", "DICompositeType", "DIDerivedType",
    "DIFile", "DIGlobalVariableExpression", "DIImportedEntity",
    "DILocalVariable", "DILocation", "DIModule", "DIStringType",
    "DISubprogram", "DISubrange", "DISubroutineType"
}

class CustomFormatter(logging.Formatter):
    """Custom formatter to add emojis based on log levels."""

    EMOJIS = {
        logging.INFO: "✅",     # Info messages
        logging.WARNING: "⚠️",  # Warnings
        logging.ERROR: "⛔",     # Errors
        logging.DEBUG: "🔍"      # Debug messages (optional)
    }

    def format(self, record):
        emoji = self.EMOJIS.get(record.levelno, "")
        record.msg = f"{emoji} {record.msg}"
        return super().format(record)

def normalize_name(name):
    """Normalize function names by stripping Fortran module mangling and trailing underscores."""
    match = re.match(r'_QM\w+P(\w+)', name)
    if match:
        return match.group(1)  # Extract only the procedure name
    return name.strip("_").rstrip("_")  # Ensure we don't accidentally remove the wrong characters

def find_object_files(directory):
    """Recursively find all .o files in the given directory."""
    return list(Path(directory).rglob("*.F.o")) + list(Path(directory).rglob("*.F90.o"))

def load_llvm_ir_files(file_list):
    """Loads LLVM IR files into memory and returns a dictionary."""
    llvm_ir_store = {}  # Dictionary to store all files
    for file_path in file_list:
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                llvm_ir_store[file_path] = f.readlines()  # Store file as a list of lines
        except UnicodeDecodeError:
            logging.error(f"Could not read file {file_path} due to encoding issues.")
    return llvm_ir_store  # Return the loaded data


def process_file(filename, llvm_ir_store, current_offset):
    max_id_in_file = 0
    updated_lines = []

    metadata_pattern = re.compile(r'!dbg !(\d+)|!(\d+)')

    logging.info(f"process_file {filename} with offset {current_offset}...")
    def replace_metadata(match):
        nonlocal max_id_in_file
        dbg_id = match.group(1)
        regular_id = match.group(2)

        if dbg_id:
            old_id = int(dbg_id)
            max_id_in_file = max(max_id_in_file, old_id)
            return f"!dbg !{old_id + current_offset}"
        elif regular_id:
            old_id = int(regular_id)
            max_id_in_file = max(max_id_in_file, old_id)
            return f"!{old_id + current_offset}"

        return match.group(0)

    for line in llvm_ir_store[filename]:
        stripped_line = line.lstrip()
        if not stripped_line or stripped_line.startswith("%"):
            continue

        updated_line = metadata_pattern.sub(replace_metadata, stripped_line)
        updated_lines.append(updated_line)

    llvm_ir_store[filename] = updated_lines

    return current_offset + max_id_in_file + 1

def resolve_type(type_id, metadata_store):

    if type_id >= len(metadata_store) or metadata_store[type_id] is None:
        logging.error(f"Type ID {type_id} not found or is None in metadata_store.")
        return "unknown"

    type_metadata = metadata_store[type_id]

    if "DIStringType" in type_metadata:
        size = type_metadata["DIStringType"].get("size", "?")
        encoding = type_metadata["DIStringType"].get("encoding", "unknown")
        return f"string[{size}]" if size else "string"

    if "DIBasicType" in type_metadata:
        return type_metadata["DIBasicType"].get("name", "unknown")

    if "DIDerivedType" in type_metadata:
        base_type_str = type_metadata["DIDerivedType"].get("baseType")
        if not base_type_str or base_type_str == "null":
    # Gracefully handle pointer to unknown base
            tag = type_metadata["DIDerivedType"].get("tag", "")
            if tag == "DW_TAG_pointer_type":
                return "ptr"
            else:
                logging.error(f"Could not resolve base type for {type_id}, metadata: {type_metadata}")
                return "unknown"
        base_type = int(base_type_str.replace("!", "")) if base_type_str else None
        return resolve_type(base_type, metadata_store) if base_type else "unknown"

    if "DICompositeType" in type_metadata:
        base_type_str = type_metadata["DICompositeType"].get("baseType")
        if not base_type_str or base_type_str == "null":
            # Fallback: return structure name
            return type_metadata["DICompositeType"].get("name", "struct")
        base_type = int(base_type_str.replace("!", "")) if base_type_str else None
        return f"Array of {resolve_type(base_type, metadata_store)}" if base_type else "unknown"

    logging.error(f"Could not resolve type for {type_id}, metadata: {type_metadata}")
    return "unknown"


def extract_metadata_entries(llvm_ir_store):
    """
    Extracts metadata entries including structured metadata and metadata lists (!{!1, !2, ...}).
    Returns:
        dict: {metadata_id (int): {metadata_type (str): {key: value}} or list}
    """
    metadata_store = {}
    logging.info("🔍 Extracting metadata entries (including metadata lists)...")

    for filename, lines in llvm_ir_store.items():
        filename = os.path.basename(filename)
        for line in lines:
            line = line.strip()
            # skip the following lines
            # "flang version 21.0.0 (https://github.com/llvm/llvm-project.git 3af6c9fa832ac29125cad76acb397d6235c371e9)"
            # "Debug Info Version"
            # "PIC Level"
            # "PIE Level"
            if "flang version" in line or "Debug Info Version" in line or "PIC Level" in line or "PIE Level" in line:
                continue

            # ✅ Case 1: Metadata list: !123 = !{!1, !2}
            list_match = re.match(r'!(\d+)\s*=\s*!\{(.*)\}', line)
            if list_match:
                metadata_id = int(list_match.group(1))
                elements_str = list_match.group(2)
                element_ids = []
                for token in elements_str.split(","):
                    token = token.strip()
                    if token.startswith("!"):
                        try:
                            element_ids.append(int(token[1:]))
                        except ValueError:
                            logging.warning(f"⚠️ Could not parse list element: {token}")
                metadata_store[metadata_id] = {
                    "list": element_ids,
                    "source_file": filename,
                    "line": line
                }
                continue

            # ✅ Case 2: Structured metadata
            struct_match = re.match(r'!(\d+)\s+=\s+(distinct\s+)?!(\w+)\((.*)', line)
            if not struct_match:
                continue

            meta_id_str, _, meta_type, body = struct_match.groups()
            metadata_id = int(meta_id_str)

            if metadata_id in metadata_store:
                logging.warning(f"⚠️ Duplicate metadata ID !{metadata_id} in file {filename}. Skipping.")
                continue

            if meta_type not in TARGET_METADATA_TYPES:
                continue

            fields = {}
            name_match = re.match(r'name:\s*"([^"]+)",\s*linkageName:\s*"([^"]+)",', body)
            if name_match:
                fields["name"], fields["linkageName"] = name_match.groups()
                body = body[name_match.end():]
            elif 'name:' in body:
                name_match = re.match(r'name:\s*"([^"]+)"', body)
                if name_match:
                    fields["name"] = name_match.group(1)
                    body = body[name_match.end():]

            kv_matches = re.findall(r'(\w+):\s*(!?\w+)', body)
            for key, val in kv_matches:
                fields[key] = val

            metadata_store[metadata_id] = {
                meta_type: fields,
                "source_file": filename,
                "line": line
            }

    return metadata_store



def extract_function_signatures(metadata_store):
    """
    Extracts subroutine argument types from DISubprogram and DISubroutineType metadata.

    Populates:
        subroutines["name"]["argType"] = list of argument types (strings)
    """
    subroutines = defaultdict(lambda: {
        "argList": [],
        "argType": [],
        "calls": defaultdict(lambda: {"name": None, "argList": [], "argTypes": []})
    })

    logging.info("🔍 Extracting function signatures from DISubprogram entries...")

# Preprocess argument names by scope to avoid repeated scanning
    arg_names_by_scope = [None] * (len(metadata_store) + 1)
    for meta in metadata_store:
        if not meta:
            continue
        if "DILocalVariable" in meta:
            var = meta["DILocalVariable"]
            scope = var.get("scope")
            if not scope or not isinstance(scope, str) or not scope.startswith("!"):
                continue
            if "arg" not in var:
                # arg keyword may be missing if it's the only one argument
                if arg_names_by_scope[scope_id] is None:
                    name = var.get("name", "arg0")
                continue
            try:
                arg_index = int(var["arg"])
                name = var.get("name", f"arg{arg_index}")
                scope_id = int(scope.replace("!", ""))
                if arg_names_by_scope[scope_id] is None:
                    arg_names_by_scope[scope_id] = []
                arg_names_by_scope[scope_id].append((arg_index, name))
#               logging.info(f"📌 Argument: {name} at scope {scope_id}")
            except ValueError:
                continue




    for meta_id, metadata in enumerate(metadata_store):
        if not isinstance(metadata, dict) or "DISubprogram" not in metadata:
            continue

        name_scope = meta_id
        sub_meta = metadata["DISubprogram"]
        if sub_meta.get("spFlags") != "DISPFlagDefinition":
            continue
        subroutine_name = sub_meta.get("linkageName") or sub_meta.get("name")

        if not subroutine_name:
            logging.error(f"Missing subroutine name for metadata ID {meta_id}")
            continue

        type_ref = sub_meta.get("type")
        if not type_ref or not isinstance(type_ref, str) or not type_ref.startswith("!"):
            logging.warning(f"No valid type reference found for subroutine {subroutine_name}")
            continue

        type_ref_id = int(type_ref.replace("!", ""))
        type_metadata = metadata_store[type_ref_id]
        if not type_metadata or "DISubroutineType" not in type_metadata:
            logging.warning(f"No DISubroutineType found for {subroutine_name} (type: {type_ref})")
            continue


        subroutine_type = type_metadata["DISubroutineType"]
        #!6 = !DISubroutineType(cc: DW_CC_normal, types: !7)
        types_entry = subroutine_type.get("types")
        types_list = []

        if not types_entry:
            logging.warning(f"No types found in DISubroutineType for {subroutine_name}")
            continue

        # Normalize types_entry into a list
        if isinstance(types_entry, str) and types_entry.startswith("!"):
            try:
                types_list_id = int(types_entry.replace("!", ""))
                #!7 = !{null, !8, !12, !8, !8, !15, !15, !15, !15}
                logging.info(f"types_list_id: {types_list_id}")
                types_list_entry = metadata_store[types_list_id] #
                if types_list_entry and "list" in types_list_entry:
                    types_list = types_list_entry["list"]
            except Exception as e:
                logging.error(f"Failed to resolve types list for {subroutine_name}: {e}")
        elif isinstance(types_entry, dict) and "list" in types_entry:
            types_list = types_entry["list"]
        elif isinstance(types_entry, list):
            types_list = types_entry

        if not isinstance(types_list, list):
            logging.warning(f"Invalid types list for subroutine {subroutine_name}: {types_entry}")
            if isinstance(types_entry, str) and types_entry.startswith("!"):
                #raw_entry = metadata_store.get(types_list_id)
                raw_entry = metadata_store[types_list_id]
                logging.error(f"Raw metadata at {types_entry} : {raw_entry}")
                logging.error(f"Metadata store: {meta_id} :  {metadata}")
                #source_file = metadata_store[meta_id].get("source_file")
                source_file = metadata[meta_id].get("source_file")
                logging.error(f"1 Source file: {source_file}")
            else:
                logging.warning(f" types_entry is not a metadata reference: {types_entry}")
            continue
        elif not types_list:
            logging.warning(f"subroutine {subroutine_name} takes no arguments")
            continue

        # Resolve argument types (excluding return type at index 0)
        arg_types = []
        for type_ref in types_list[1:]:
            if isinstance(type_ref, str) and type_ref.startswith("!"):
                try:
                    type_id = int(type_ref.replace("!", ""))
                    resolved = resolve_type(type_id, metadata_store)
                    if resolved == "unknown":
                        logging.error(f"Could not resolve type {type_ref} for subroutine {subroutine_name}")
                        logging.error(f"Metadata contents: {metadata}")
                        source_file = metadata_store[meta_id].get("source_file")
                        logging.error(f"2 Source file: {source_file}")

                    arg_types.append(resolved)
                except Exception as e:
                    logging.error(f"Failed to resolve type {type_ref} for subroutine {subroutine_name}: {e}")
                    arg_types.append("unknown")
            elif isinstance(type_ref, int):
                resolved = resolve_type(type_ref, metadata_store)
                if resolved == "unknown":
                    logging.error(f"Could not resolve type {type_ref} for subroutine {subroutine_name}")
                    logging.error(f"Metadata contents: {metadata}")
                    source_file = metadata_store[meta_id].get("source_file")
                    logging.error(f"3 Source file: {source_file}")

                arg_types.append(resolved)
            else:
                arg_types.append("unknown")

#       # 1. Try to get scope ID of the current subroutine
#       scope_str = sub_meta.get("scope")
#       if scope_str and scope_str.startswith("!"):
#           scope_id = int(scope_str.replace("!", ""))
#       else:
#           scope_id = None
        arg_names = []
        scope_id = name_scope
        if scope_id is not None and scope_id < len(arg_names_by_scope) and arg_names_by_scope[scope_id] is not None:
            sorted_args = sorted(arg_names_by_scope[scope_id] or [])
            arg_names = [name for _, name in sorted_args]
        else:
            logging.error(f"Could not resolve scope ID for subroutine {subroutine_name}, scope_id: {scope_id}")
            logging.error(f"Metadata contents: {metadata}")
            logging.error(f"arg_names_by_scope: {arg_names_by_scope[scope_id]}")
            #exit the program
            #exit(1)



        # 4. Store names and types
        subroutines[subroutine_name]["argList"] = arg_names
        subroutines[subroutine_name]["argType"] = arg_types
        logging.info(f"📌 Subroutine: {subroutine_name} → Arguments: {arg_types}")
        logging.info(f"📌 Subroutine: {subroutine_name} → Arguments: {arg_names}")



    return subroutines

#def extract_function_signatures(metadata_store):
#    """
#    Extracts subroutine argument types from DISubprogram and DISubroutineType metadata.
#    Only keeps actual definitions (spFlags = DISPFlagDefinition).
#    Returns a dictionary: subroutines[name] = {argList, argType, calls}.
#    """
#    from collections import defaultdict
#
#    subroutines = defaultdict(lambda: {
#        "argList": [],
#        "argType": [],
#        "calls": defaultdict(lambda: {"name": None, "argList": [], "argTypes": []})
#    })
#
#    logging.info("🔍 Extracting function signatures from DISubprogram entries...")
#
#    # Index local variables by scope
#    scope_to_args = defaultdict(list)
#    for meta in metadata_store.values():
#        if "DILocalVariable" in meta:
#            var = meta["DILocalVariable"]
#            scope = var.get("scope")
#            if scope and "arg" in var:
#                try:
#                    arg_index = int(var["arg"])
#                    scope_id = int(scope.replace("!", ""))
#                    scope_to_args[scope_id].append((arg_index, var.get("name", f"arg{arg_index}")))
#                except ValueError:
#                    continue
#
#    for meta_id, metadata in metadata_store.items():
#        if "DISubprogram" not in metadata:
#            continue
#
#        sub_meta = metadata["DISubprogram"]
#
#        # ✅ Only include actual definitions
#        if sub_meta.get("spFlags") != "DISPFlagDefinition":
#            continue
#
#        subroutine_name = sub_meta.get("linkageName") or sub_meta.get("name")
#        if not subroutine_name:
#            logging.error(f"Missing subroutine name for metadata ID {meta_id}")
#            continue
#
#        # ✅ Prevent duplicates (keep only first definition)
#        if subroutine_name in subroutines and subroutines[subroutine_name]["argType"]:
#            continue
#
#        # -- Resolve DISubroutineType
#        type_ref = sub_meta.get("type")
#        if not (isinstance(type_ref, str) and type_ref.startswith("!")):
#            continue
#
#        type_ref_id = int(type_ref[1:])
#        type_metadata = metadata_store.get(type_ref_id, {})
#        subroutine_type = type_metadata.get("DISubroutineType", {})
#        types_entry = subroutine_type.get("types")
#
#        types_list = []
#        if isinstance(types_entry, str) and types_entry.startswith("!"):
#            types_list_id = int(types_entry[1:])
#            entry = metadata_store.get(types_list_id, {})
#            types_list = entry.get("list", [])
#        elif isinstance(types_entry, list):
#            types_list = types_entry
#        elif isinstance(types_entry, dict) and "list" in types_entry:
#            types_list = types_entry["list"]
#
#        if not isinstance(types_list, list):
#            continue
#
#        # -- Resolve argument types (excluding return type)
#        arg_types = []
#        for type_ref in types_list[1:]:
#            try:
#                if isinstance(type_ref, str) and type_ref.startswith("!"):
#                    type_id = int(type_ref[1:])
#                elif isinstance(type_ref, int):
#                    type_id = type_ref
#                else:
#                    raise ValueError
#                arg_types.append(resolve_type(type_id, metadata_store))
#            except Exception:
#                arg_types.append("unknown")
#
#        # -- Resolve argument names from scope
#        scope_str = sub_meta.get("scope")
#        if scope_str and scope_str.startswith("!"):
#            scope_id = int(scope_str[1:])
#            args = sorted(scope_to_args.get(scope_id, []))
#            arg_names = [name for _, name in args]
#        else:
#            arg_names = []
#
#        subroutines[subroutine_name]["argList"] = arg_names
#        subroutines[subroutine_name]["argType"] = arg_types
#        logging.info(f"📌 Subroutine: {subroutine_name} → Arguments: {arg_types}")
#
#    return subroutines



def extract_call_sites(llvm_ir_store, metadata_store, subroutines, scope_map):
    """
    Extracts call statements from FIR and matches them to caller subroutines.
    Populates:
        subroutines["caller"]["calls"][line] = {
            "name": callee,
            "argList": [args],
            "argTypes": [types]
        }
    """
    logging.info("🔍 Extracting function call sites...")

    call_pattern = re.compile(r'fir\.call\s+@(\w+)\((.*?)\)\s+loc\("(.+?):(\d+):\d+"\)')

    for filename, lines in llvm_ir_store.items():
        for line in lines:
            match = call_pattern.search(line)
            if not match:
                continue

            callee, raw_args, source_file, line_num = match.groups()
            line_num = int(line_num)

            # **Resolve Caller Subroutine Using Debug Location**
            caller = None
            for subroutine, data in subroutines.items():
                if data.get("source_file") == source_file:
                    caller = subroutine
                    break

            if not caller:
                logging.error(f"Could not resolve caller for {callee} at {source_file}:{line_num}")
                continue

            # **Extract Argument List**
            arg_list = [arg.strip() for arg in raw_args.split(",") if arg.strip()]
            arg_types = []

            for arg in arg_list:
                # **Case 1: Constants**
                if re.match(r'f\d+\s+[+-]?\d+(\.\d+)?', arg):  # Matches `f64 9.81`
                    arg_types.append(arg.split()[0])  # Extracts type (`f64`, `i32`, etc.)

                # **Case 2: Caller Arguments**
                elif arg.startswith("%arg"):
                    arg_index = int(arg[4:])
                    if arg_index < len(subroutines[caller]["argTypes"]):
                        arg_types.append(subroutines[caller]["argTypes"][arg_index])
                    else:
                        arg_types.append("unknown")

                # **Case 3: Local Variables**
                elif arg.startswith("%"):
                    resolved_type = "unknown"
                    for alloc_line in lines:
                        if f"fir.alloca {arg} :" in alloc_line:
                            type_match = re.search(r'fir.alloca\s+%\w+\s+:\s+(!\S+)', alloc_line)
                            if type_match:
                                resolved_type = type_match.group(1)
                            break
                    arg_types.append(resolved_type)

                # **Case 4: Global Variables**
                elif arg.startswith("@"):
                    global_name = arg
                    resolved_type = "unknown"
                    for global_line in lines:
                        if f"fir.global {global_name}" in global_line:
                            type_match = re.search(r'fir.global\s+@\w+\s+:\s+(!\S+)', global_line)
                            if type_match:
                                resolved_type = type_match.group(1)
                            break
                    arg_types.append(resolved_type)

                # **Default Case**
                else:
                    arg_types.append("unknown")

            # ✅ **Store Call Information**
            subroutines[caller]["calls"][line_num] = {
                "name": callee,
                "argList": arg_list,
                "argTypes": arg_types
            }

            logging.info(f"📞 in {caller} at line {line_num}: {callee}({arg_types})")



def build_scope_map(metadata_store):
    """
    Creates a mapping from scope ID to subroutine name by parsing DISubprogram entries.
    Returns a dictionary { scope_id -> subroutine_name }.
    """
    scope_map = {}

    for meta_id, metadata in enumerate(metadata_store):
        if isinstance(metadata, dict) and "DISubprogram" in metadata:
#   for meta_id, metadata in metadata_store.items():
#       if "DISubprogram" in metadata:
            sub_meta = metadata["DISubprogram"]

            if sub_meta.get("spFlags") != "DISPFlagDefinition":
               continue

            subroutine_name = metadata["DISubprogram"].get("linkageName") or metadata["DISubprogram"].get("name")

            # 🚨 DEBUG: Check if `name` and `linkageName` exist
            if "linkageName" not in metadata["DISubprogram"] and "name" not in metadata["DISubprogram"]:
                logging.error(f" Missing both 'linkageName' and 'name' for DISubprogram at scope {meta_id}")
                logging.error(f" Metadata contents: {metadata['DISubprogram']}")
                continue

            # 🚨 DEBUG: Log unexpected None values
            if subroutine_name is None:
                logging.critical(f"Subroutine name is 'None' for scope {meta_id}")
                logging.critical(f"Metadata contents: {metadata['DISubprogram']}")
                raise ValueError(f"Unexpected None for subroutine name in scope {meta_id}")

            scope_id = meta_id
            scope_map[scope_id] = subroutine_name

            # Corrected empty check
            if len(subroutine_name) == 0:
                logging.error(f"Subroutine name is EMPTY for scope {scope_id}")
            
            logging.info(f"Found subroutine: {subroutine_name} with scope {scope_id}")

    return scope_map




#def extract_metadata_entries(llvm_ir_store):
#    """
#    Extracts metadata entries dynamically and stores them indexed by metadata ID.
#
#    Returns:
#        dict: {metadata_id: {metadata_type: {key: value, ...}}}
#    """
#    metadata_store = defaultdict(dict)  # FIX: Ensures multiple metadata types per ID
#
#    logging.info("Extracting all metadata entries dynamically...")
#
#    for filename, lines in llvm_ir_store.items():
#        for line in lines:
#            # Match metadata definition lines like `!1234 = !DISubprogram(...)`
#            match = re.match(r'!(\d+)\s+=\s+(distinct\s+)?!(\w+)\((.*)', line)
#            if not match:
#                continue  # Skip lines that don't define metadata
#
#            id_str, distinct_kw, metadata_type, rest_of_line = match.groups()
#            metadata_id = int(id_str)  # FIX: Ensure metadata_id is stored as an integer
#
#            # Extract fields dynamically
#            fields = {}
#
#            # Special handling for `name` and `linkageName` (appear first)
#            name_match = re.match(r'name:\s*"([^"]+)",\s*linkageName:\s*"([^"]+)",', rest_of_line)
#            if name_match:
#                fields["name"], fields["linkageName"] = name_match.groups()
#                rest_of_line = rest_of_line[name_match.end():]  # Remove processed part
#            elif 'name:' in rest_of_line:
#                # If only `name` is available without `linkageName`
#                name_match = re.match(r'name:\s*"([^"]+)"', rest_of_line)
#                if name_match:
#                    fields["name"] = name_match.group(1)
#                    rest_of_line = rest_of_line[name_match.end():]
#
#            # Extract key-value pairs dynamically
#            key_value_matches = re.findall(r'(\w+):\s*(!?\w+)', rest_of_line)
#
#            for key, value in key_value_matches:
#                fields[key] = value  # Store field in dictionary
#
#            # FIX: Store metadata **by type** under each `!X`
#            if metadata_type not in metadata_store[metadata_id]:
#                metadata_store[metadata_id][metadata_type] = {}
#
#            metadata_store[metadata_id][metadata_type].update(fields)
#
#            # Debug logs
#            if metadata_type == "DISubprogram":
#                logging.info(f"Subroutine: {metadata_id} with fields {fields}")
#
#    return metadata_store  # FIXED: Now stores multiple metadata types per `!X`

def compute_max_metadata_id(lines):
    max_id = 0
    for line in lines:
        for match in re.finditer(r'!(\d+)', line):
            max_id = max(max_id, int(match.group(1)))
    return max_id


def print_subroutine_signatures(subroutines, metadata_store):
    print("\n📘 Subroutine Signatures:\n")

    for name, info in subroutines.items():
        # Attempt to find line number and file
        line_number = "unknown"
        source_file = "unknown"
#       for meta_id, data in metadata_store.items():
#           if "DISubprogram" in data:?s

        for meta_id, data in enumerate(metadata_store):
            if not isinstance(data, dict) or "DISubprogram" not in data:
                continue
            sub_name = data["DISubprogram"].get("linkageName", data["DISubprogram"].get("name"))
            if sub_name == name:
                line_number = data["DISubprogram"].get("line", "unknown")
                source_file = data.get("source_file", "unknown")
                break

        print(f"📌 Subroutine: {name}  (defined at {source_file} : {line_number})")

        arg_types = info.get("argType", [])
        arg_names = info.get("argList", [])

        if not arg_types:
            print("   └─ No arguments\n")
            continue

        print("   ├─ Arguments:")
        for idx, arg_type in enumerate(arg_types):
            arg_name = arg_names[idx] if idx < len(arg_names) else f"arg{idx+1}"
            print(f"   │   • {arg_name:<10} : {arg_type}")
        print()





if __name__ == "__main__":

    formatter = CustomFormatter("%(levelname)s: %(message)s")
    handler = logging.StreamHandler()
    handler.setFormatter(formatter)
    logging.basicConfig(level=logging.INFO, handlers=[handler])

    # Set the root directory where .o files are stored
    root_dir = "../engine/cbuild_engine_linux64_flang_db/"
    # root_dir = "./"

    logging.info(f"Searching for .o files in {root_dir}...")
    object_files = find_object_files(root_dir)

    if not object_files:
        logging.error("No .o files found.")
        exit(1)

    logging.info(f"📂 Found {len(object_files)} .o files. Parsing them now...")

    llvm_ir_store = load_llvm_ir_files(object_files)  # Load all files into memory
    current_max_x = 0  # Track max !X value globally
    all_metadata = {}



    for obj_file in object_files:
        try:
            logging.info(f"📜 Parsing {obj_file} ...")
            new_max_x = process_file(obj_file, llvm_ir_store, current_max_x)
            current_max_x = new_max_x+1  # Update max value

        except UnicodeDecodeError as e:
            logging.error(f"UnicodeDecodeError in file: {obj_file} → {e}")

    metadata_store = extract_metadata_entries(llvm_ir_store)
    max_metadata_id = max(metadata_store.keys())
    #create a list of size max_metadata_id+1
    metadata_list = [None] * (max_metadata_id+1)
    #fill the list with the values of the dictionnary
    for key, value in metadata_store.items():
        metadata_list[key] = value

    scope_map = build_scope_map(metadata_list)
    subroutines = extract_function_signatures(metadata_list)
    #convert the metadata_store dictionnary to a list. metadata_store is a dictionnary:
    # the keys are integers from 0 to maxvalue of metadata id, with is almost dense, only a few keys are missing
    #get the max value of the keys

    extract_call_sites(llvm_ir_store, metadata_list, subroutines, scope_map)
    print_subroutine_signatures(subroutines, metadata_list)

    logging.info("Parsing complete.")
