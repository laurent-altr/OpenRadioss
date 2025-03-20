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

def process_file(filename, llvm_ir_store, current_max_x):
    """Processes an LLVM IR file, updating metadata IDs in a single pass."""
    new_max_x = current_max_x  # Keep track of max metadata ID
    updated_lines = []

    # Precompile regex to match `!X` and `!dbg !X`
    metadata_pattern = re.compile(r'(!dbg )?!\d+')

    logging.info(f"Processing {filename}..., offset: {current_max_x}")

    for line in llvm_ir_store[filename]:
        stripped_line = line.lstrip()  # Remove only leading spaces (not trailing)
        
        # Skip empty lines & lines where first non-space char is '%'
        if not stripped_line or stripped_line.startswith("%"):
            continue  

        # **Replace !X → !Y where Y = X + current_max_x**
        def replace_metadata(match):
            token = match.group(0)  # Get full match (!dbg !X or !X)
            prefix = ""

            if token.startswith("!dbg "):
                prefix = "!dbg "
                token = token[len(prefix):]  # Remove "!dbg " prefix

            old_id = int(token[1:])  # Extract numeric part from `!X`
            new_id = old_id + current_max_x  # Apply offset
            nonlocal new_max_x
            new_max_x = max(new_max_x, new_id)

            return f"{prefix}!{new_id}"  # Preserve "!dbg " prefix

        updated_line = metadata_pattern.sub(replace_metadata, stripped_line)

        # Store the updated line
        updated_lines.append(updated_line)

    # Batch update `llvm_ir_store`
    llvm_ir_store[filename] = updated_lines

    return new_max_x  # Return updated max metadata ID for next file


def resolve_type(type_id, metadata_store):
    """
    Resolves the type name for a given metadata ID (!X) by checking DIBasicType, DIDerivedType, or DICompositeType.
    """
    if type_id not in metadata_store:
        logging.error(f"Type ID {type_id} not found in metadata_store.")
        return "unknown"

    type_metadata = metadata_store[type_id]

    # Check if it's a basic type
    if "DIBasicType" in type_metadata:
        return type_metadata["DIBasicType"].get("name", "unknown")

    # Check if it's a derived type
    if "DIDerivedType" in type_metadata:
        base_type = type_metadata["DIDerivedType"].get("baseType")
        return resolve_type(base_type, metadata_store) if base_type else "unknown"

    # Check if it's a composite type (array, struct, etc.)
    if "DICompositeType" in type_metadata:
        base_type = type_metadata["DICompositeType"].get("baseType")
        return f"Array of {resolve_type(base_type, metadata_store)}" if base_type else "unknown"

    logging.error(f"Could not resolve type for {type_id}, metadata: {type_metadata}")
    return "unknown"

def extract_function_signatures(metadata_store):
    """
    Extracts subroutine argument names and types from DISubprogram and DISubroutineType.

    Populates subroutines["name"]["argList"] and subroutines["name"]["argType"].
    """
    subroutines = defaultdict(lambda: {"argList": [], "argType": [], "calls": defaultdict(lambda: {"name": None, "argList": [], "argTypes": []})})

    logging.info("🔍 Extracting function signatures from DISubprogram entries...")

    for meta_id, metadata in metadata_store.items():
        if "DISubprogram" in metadata:
            subroutine_name = metadata["DISubprogram"].get("linkageName", metadata["DISubprogram"].get("name"))
            type_ref = metadata["DISubprogram"].get("type")

            if type_ref and type_ref in metadata_store:
                function_type = metadata_store[type_ref]
                if "DISubroutineType" in function_type:
                    type_list = function_type["DISubroutineType"].get("types", [])

                    arg_types = []
                    for type_id in type_list[1:]:  # Exclude return type
                        resolved_type = resolve_type(type_id, metadata_store)
                        arg_types.append(resolved_type)

                    subroutines[subroutine_name]["argType"] = arg_types

            logging.info(f"Found subroutine: {subroutine_name}, Arguments: {subroutines[subroutine_name]['argType']}")

    return subroutines


def extract_call_sites(llvm_ir_store, metadata_store, subroutines, scope_map):
    """
    Extracts call statements from LLVM IR and matches them to caller subroutines.
    Populates subroutines["caller"]["calls"][line] with callee, arguments, and argument types.
    """
    logging.info("🔍 Extracting function call sites...")

    call_pattern = re.compile(r'call .*? @(\w+)\((.*?)\), !dbg !(\d+)')

    for filename, lines in llvm_ir_store.items():
        for line in lines:
            match = call_pattern.search(line)
            if match:
                callee, raw_args, dbg_id = match.groups()
                #delete all "!" from dbg_id
                dbg_id = dbg_id.replace("!", "")
                dbg_key = int(dbg_id)

                # Resolve caller subroutine from scope map
                if dbg_key in metadata_store and "DILocation" in metadata_store[dbg_key]:
                    scope_ref_str = metadata_store[dbg_key]["DILocation"].get("scope")
                    scope_ref = int(scope_ref_str.replace("!", "")) if scope_ref_str else None
                    if scope_ref in scope_map:
                        caller = scope_map[scope_ref]
                    else:
                        logging.error(f"Could not resolve caller for !dbg {dbg_id}")
                        logging.error(f"line: {line}")
                        logging.error(f"metadata: {metadata_store[dbg_key]}")

                        continue
                else:
                    logging.error(f"Debug location not found for !dbg {dbg_id}")
                    continue

                # Extract arguments
                arg_list = [arg.strip() for arg in raw_args.split(",") if arg.strip()]
                arg_types = []

                for arg in arg_list:
                    if arg.startswith("ptr %"):  # Variable reference
                        var_ref = arg.replace("ptr ", "")
                        if var_ref in metadata_store:
                            var_metadata = metadata_store[var_ref]
                            if "DILocalVariable" in var_metadata:
                                var_type_id = var_metadata["DILocalVariable"].get("type")
                                if var_type_id in metadata_store:
                                    var_type = metadata_store[var_type_id].get("DIBasicType", {}).get("name", "unknown")
                                    arg_types.append(var_type)
                                else:
                                    arg_types.append("unknown")
                        else:
                            arg_types.append("unknown")
                    elif arg.isdigit():  # Constant integer
                        arg_types.append("integer")
                    else:
                        arg_types.append("unknown")

                # Store call information
                fortran_line = metadata_store[dbg_key]["DILocation"].get("line", "unknown")
                subroutines[caller]["calls"][fortran_line] = {
                    "name": callee,
                    "argList": arg_list,
                    "argTypes": arg_types
                }

                logging.info(f"📞in {caller} at line {fortran_line}: {callee}({arg_types})")



def build_scope_map(metadata_store):
    """
    Creates a mapping from scope ID to subroutine name by parsing DISubprogram entries.
    Returns a dictionary { scope_id -> subroutine_name }.
    """
    scope_map = {}
    for meta_id, metadata in metadata_store.items():
        if "DISubprogram" in metadata:
            subroutine_name = metadata["DISubprogram"].get("linkageName", metadata["DISubprogram"].get("name"))
            scope_id = meta_id
            scope_map[scope_id] = subroutine_name
            logging.info(f"Found subroutine: {subroutine_name} with scope {scope_id}")

    return scope_map


def extract_metadata_entries(llvm_ir_store):
    """
    Extracts metadata entries dynamically and stores them indexed by metadata ID.

    Returns:
        dict: {metadata_id: {metadata_type: {key: value, ...}}}
    """
    metadata_store = {}

    logging.info("Extracting all metadata entries dynamically...")

    for filename, lines in llvm_ir_store.items():
        for line in lines:
            match = re.match(r'!(\d+)\s+=\s+(distinct\s+)?!(\w+)\((.*)', line)
            if not match:
                continue  # Skip lines that don't define metadata

#           metadata_id, metadata_type, rest_of_line = match.groups()
            id, distinct_kw, metadata_type, rest_of_line = match.groups()

            metadata_id = int(id.replace("!", ""))
#           metadata_id = f"!{metadata_id}"
#           if metadata_id =="!0": zero seems to be a valid ID
#               logging.error(f"Found metadata ID 0 in line: {line}")

            # Skip if the metadata type is not in our list
            if metadata_type not in TARGET_METADATA_TYPES:
                #search if each TARGET_METADATA_TYPES are in the line
                for target_metadata_type in TARGET_METADATA_TYPES:
                    if target_metadata_type in line:
                        logging.error(f"Found {target_metadata_type} in line: {line}, but not in metadata_store: {metadata_store}")
                continue

            # Extract fields dynamically
            fields = {}

            # Special handling for `name` and `linkageName` (appear first)
            name_match = re.match(r'name:\s*"([^"]+)",\s*linkageName:\s*"([^"]+)",', rest_of_line)
            if name_match:
                fields["name"], fields["linkageName"] = name_match.groups()
                rest_of_line = rest_of_line[name_match.end():]  # Remove processed part

            elif 'name:' in rest_of_line:
                # If only `name` is available without `linkageName`
                name_match = re.match(r'name:\s*"([^"]+)"', rest_of_line)
                if name_match:
                    fields["name"] = name_match.group(1)
                    rest_of_line = rest_of_line[name_match.end():]

            # Extract key-value pairs dynamically
            key_value_matches = re.findall(r'(\w+):\s*(!?\w+)', rest_of_line)

            for key, value in key_value_matches:
                fields[key] = value  # Store field in dictionary

            # Store metadata indexed by metadata_id first, then metadata_type
            if metadata_id not in metadata_store:
                metadata_store[metadata_id] = {}

            # how to get the type of metadata_id. It needs to be an integer, not a string




            metadata_store[metadata_id][metadata_type] = fields

#           if "4747836" in line:
#               logging.info(f"ID FOUND {metadata_type}: {metadata_id} with fields {fields}")

            if metadata_type == "DISubprogram":
                logging.info(f"Subroutine: {metadata_id} with fields {fields}")


       #     logging.info(f"Found {metadata_type}: {metadata_id} with fields {fields}")

    return metadata_store



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
            current_max_x = new_max_x  # Update max value
        except UnicodeDecodeError as e:
            logging.error(f"UnicodeDecodeError in file: {obj_file} → {e}")

    metadata_store = extract_metadata_entries(llvm_ir_store)
    scope_map = build_scope_map(metadata_store)
    subroutines = extract_function_signatures(metadata_store)
    extract_call_sites(llvm_ir_store, metadata_store, subroutines, scope_map)


    logging.info("Parsing complete.")
