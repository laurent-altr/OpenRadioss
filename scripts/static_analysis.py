#!/usr/bin/env python3
import re
import sys
from pathlib import Path
from collections import defaultdict
import logging
import hashlib


class CustomFormatter(logging.Formatter):
    """Custom formatter to add emojis based on log levels."""
    EMOJIS = {
        logging.INFO: "",     # Info messages
        logging.WARNING: "",  # Warnings
        logging.ERROR: "",     # Errors
        logging.DEBUG: "",     # Debug messages
        logging.CRITICAL: ""   # Added critical level
    }
    
    def format(self, record):
        emoji = self.EMOJIS.get(record.levelno, "")
        record.msg = f"{emoji} {record.msg}"
        return super().format(record)


class MessageCollector(logging.Handler):
    """Custom handler that stores log messages for later display."""
    
    def __init__(self):
        super().__init__()
        self.messages = defaultdict(list)
        self.suppressed_hashes = set()
        self.raw_messages = defaultdict(list)  # Store raw messages by level
        
        # Load suppressed errors from file if it exists
#       if suppression_file and Path(suppression_file).exists():
#           with open(suppression_file, 'r') as f:
#               for line in f:
#                   if line:  # Skip empty lines
#                       # Create hash of the error message
#                       #remove all white spaces from the line
#                       message_hash = hashlib.md5(line.encode()).hexdigest()
#                       self.suppressed_hashes.add(message_hash)
#       
    def emit(self, record):
        # Get the raw message (without emoji)
        raw_msg = record.getMessage()
        
        # Create a hash for this message
        msg_hash = hashlib.md5(raw_msg.encode()).hexdigest()
        
        # Store the message regardless of suppression status
        level_name = record.levelname
        formatted_msg = self.format(record)
        
        # Store raw message for potential future use
        self.raw_messages[level_name].append(raw_msg)
        
        # Store formatted message (with emoji) for display
        # Mark messages that are in the suppression file for the summary
        is_suppressed = msg_hash in self.suppressed_hashes
        self.messages[level_name].append((formatted_msg, is_suppressed))
        
    def get_summary(self):
        """Return a summary of all new messages (not in suppression file)."""
        summary = []
        
        # Count new messages (not suppressed)
        new_message_count = sum(
            len([msg for msg, suppressed in msgs if not suppressed]) 
            for msgs in self.messages.values()
        )
        
        if new_message_count == 0:
            return "No new issues found! All issues are in the suppression file."
            
        
        # Order by severity
        for level in ["CRITICAL", "ERROR"]:
            if level in self.messages:
                new_msgs = [msg for msg, suppressed in self.messages[level] if not suppressed]
                if new_msgs:
                    for msg in new_msgs:
                        summary.append(f"  {msg}")
                    
        return "\n".join(summary)


def fortranize_type(type_str):

#    !fir.ref<f64> -> real(8)
#    !fir.ref<i32> -> integer(4)
#    !fir.ref<i64> -> integer(8)
    if type_str == "!fir.ref<f32>":
        return "real(4)"
    if type_str == "!fir.ref<f64>":
        return "real(8)"
    if type_str == "!fir.ref<i32>":
        return "integer(4)"
    if type_str == "!fir.ref<i64>":
        return "integer(8)"

#remove "fir." from the type
    new_type = type_str.replace("!fir.ref", "")
# remove .type from the type
    new_type = new_type.replace("!fir.type", "")
#   replace "<" and ">" by ""
    new_type = new_type.replace("<", "")
    new_type = new_type.replace(">", "")
# remove "_QM" from the type
    new_type = new_type.replace("_QM", "")
    new_type = new_type.replace("_QF", "")
    new_type = new_type.replace("T", "%")

    #count the number of "?x" in the type
    count = new_type.count("?x")
    
    #replace "!fir.array<" by "array
    #dimname = 1D,2D,3D depending on the number of "?x"
    array_name = f"array({count}D)"
    new_type = new_type.replace("!fir.array",array_name)
    #remove all the "?x" from the type
    new_type = new_type.replace("?x", "")

    #replace i32 by integer(4)
    new_type = new_type.replace("i32", " integer(4)")
    new_type = new_type.replace("i64", " integer(8)")
    new_type = new_type.replace("f32", " real(4)")
    new_type = new_type.replace("f64", " real(8)")
    #
    return new_type
def fortranize_name(name):
    """Convert a name to Fortran style."""
    new_name = name.replace("_QM", "")
    new_name = new_name.replace("_QF", "")
    new_name = new_name.replace("_Q", "")
    new_name = new_name.replace("P", ".")
    new_name = new_name.replace("T", "%")
    return new_name


# === Type Simplifier ===
def simplify_type(type_str):
    """Simplify nested !fir.type<...{...}> into !fir.type<...>."""
    result = []
    i = 0
    while i < len(type_str):
        if type_str.startswith("!fir.type<", i):
            result.append("!fir.type<")
            i += len("!fir.type<")
            name_buf = ""
            while i < len(type_str):
                c = type_str[i]
                if c == '{':
                    break
                name_buf += c
                i += 1
            result.append(name_buf.strip())
            if i < len(type_str) and type_str[i] == '{':
                depth = 1
                i += 1
                while i < len(type_str) and depth > 0:
                    if type_str[i] == '{':
                        depth += 1
                    elif type_str[i] == '}':
                        depth -= 1
                    i += 1
            result.append(">")
        else:
            result.append(type_str[i])
            i += 1
    return ''.join(result)

def split_fir_arguments(arglist):
    """Split arguments at top-level commas, respecting nested brackets/braces."""
    args = []
    buf = ''
    stack = []
    for c in arglist:
        if c == ',' and not stack:
            if buf.strip():
                args.append(buf.strip())
                buf = ''
        else:
            buf += c
            if c in ('<', '{'):
                stack.append(c)
            elif c in ('>', '}'):
                if stack:
                    stack.pop()
    if buf.strip():
        args.append(buf.strip())
    return args

def parse_argument(arg):
    name_match = re.search(r'fir\.bindc_name\s*=\s*"([^"]+)"', arg)
    type_match = re.search(r'%\w+:\s+([^ ]+)', arg)
    raw_type = type_match.group(1).strip() if type_match else "(unknown)"
    simplified_type = simplify_type(raw_type)
    arg_name = name_match.group(1).strip() if name_match else "(anonymous)"
    return simplified_type, arg_name

def parse_call_arguments(type_list_str):
    args = split_fir_arguments(type_list_str)
    return [simplify_type(arg) for arg in args]

# === Main Parser ===
def process_fir_file(fir_path: Path):
    if not fir_path.exists():
        logging.error(f"Error: File not found: {fir_path}")
        sys.exit(1)

    with open(fir_path, "r") as f:
        lines = f.readlines()

    data = dict()
    current_func = None
    call_counters = defaultdict(lambda: defaultdict(int))

    func_pattern = re.compile(r"func\.func\s+@(\S+)\((.*)\)")
    call_pattern = re.compile(r"^fir\.call\s+@(\S+)\([^\)]*\)(?:\s+\S+<[^>]+>)*\s*:\s*\((.*?)\)\s*->")

    for lineno, line in enumerate(lines):
        line = line.strip()

        # Skip private declarations
        if "func.func private" in line:
            continue

        # Start of a function definition
        m = func_pattern.search(line)
        
        if m:
            current_func = m.group(1)
            raw_arglist = m.group(2)
            args = split_fir_arguments(raw_arglist)
            arg_types = []
            arg_names = []
            for arg in args:
                t, n = parse_argument(arg)
                arg_types.append(t)
                arg_names.append(n)
            data[current_func] = {
                "ArgNames": arg_names,
                "ArgTypes": arg_types,
                "Callees": defaultdict(dict)
            }
            continue

        # Match a fir.call only if we're inside a function
        if current_func:
            cm = call_pattern.search(line)
            if cm:
                callee = cm.group(1)
                # skip it the function name starts with "llvm"
                if callee.startswith("llvm"):
                    continue
                # skip it if the function name starts with _Fortran
                if callee.startswith("_Fortran"):
                    continue
                arglist = cm.group(2)
                simplified_args = parse_call_arguments(arglist)
                count = call_counters[current_func][callee]
                data[current_func]["Callees"][callee][count] = {
                    "ArgTypes": simplified_args
                    # ArgNames not known at call site
                }
                call_counters[current_func][callee] += 1

    return data

def find_object_files(directory):
    """Recursively find all .o files in the given directory."""
    return list(Path(directory).rglob("*.fir")) + list(Path(directory).rglob("*.fir"))

if __name__ == "__main__":

    root_logger = logging.getLogger()
    root_logger.setLevel(logging.ERROR)
    
    # Console handler for immediate display
    formatter = CustomFormatter("%(levelname)s: %(message)s")

    console = logging.StreamHandler()
    console.setFormatter(formatter)
    root_logger.addHandler(console)
    
    # Create our collector for summary
    collector = MessageCollector()
    collector.setFormatter(formatter)   
    root_logger.addHandler(collector)
        
    
    handler = logging.StreamHandler()
    logging.basicConfig(level=logging.ERROR, handlers=[handler])

    # Set the root directory where .o files are stored
    # if argument is "starter", use the starter directory
    software = "starter"
    if len(sys.argv) > 1 and sys.argv[1] == "engine":
        root_dir = "../engine/cbuild_engine_linux64_flang_analysis/CMakeFiles/engine_linux64_flang_analysis.dir"
        software = "engine"
    else: #if argument is not starter, then it is "engine"
        root_dir = "../starter/cbuild_starter_linux64_flang_analysis/CMakeFiles/starter_linux64_flang_analysis.dir"
        software = "starter"

    #check if root_dir exists
    if not Path(root_dir).exists():
        logging.critical(f"Error: Directory not found: {root_dir}")
        exit(1)
    # root_dir = "./"

    logging.info(f"Searching for .o files in {root_dir}...")
    object_files = find_object_files(root_dir)
    if not object_files:
        logging.critical("No .fir files found.")
        exit(1)
    #count the number of .fir files
    print(f"Number of .fir files found: {len(object_files)}")

    call_data = dict()
    for obj_file in object_files:
        try:
            logging.info(f"Parsing {obj_file} ...")
            call_data_local = process_fir_file(obj_file)
            # combine the data
            for fname, info in call_data_local.items():
                if fname in call_data:
                    call_data[fname]["Callees"].update(info["Callees"])
                else:
                    call_data[fname] = info


        except UnicodeDecodeError as e:
            logging.error(f"UnicodeDecodeError in file: {obj_file} → {e}")

    # Display the structure
    # compile regular expressions for normalizing function names
    normalized_fname_re = re.compile(r"^_QP")
    # replace a sequence of "?x" by only one "?x"
    dimension_re = re.compile(r'(\?x)+')
    dimension2_re = re.compile(r'\d+x')
    #remove fir.array<?x
    fir_array_re = re.compile(r'\!fir\.array<\?x')
    #replace one trailing ">>" by ">""
    trailing_re = re.compile(r'>>')
    for fname, info in call_data.items():
        # remove leading "_QP" from the function name
        #normalized_fname = normalized_fname_re.sub("", fname)
        normalized_fname = fortranize_name(fname)

        logging.info(f"\nFunction: {normalized_fname}")
        for i, (t, n) in enumerate(zip(info["ArgTypes"], info["ArgNames"])):
            logging.info(f"    arg[{i}]: {t}  name: {n}")

        if info["Callees"]:
            logging.info("  Calls:")
            for callee, instances in info["Callees"].items():
                #normalized_callee = normalized_fname_re.sub("", callee)
                normalized_callee = fortranize_name(callee)
                for callnum, callinfo in instances.items():
                    logging.info(f"    -> {normalized_callee} (call #{callnum}):")
                    for i, t in enumerate(callinfo["ArgTypes"]):
                        logging.info(f"         arg[{i}]: {t}")
                        #checks if the arguments of the Call are the same as the arguments of the function
                        #check if info["ArgTypes"] == call_data[callee]["ArgTypes"]
                    callee_name = callee
                    #check if callee_name is in the call_data
                    if callee_name in call_data:
                      #same number of arguments
                        if callinfo["ArgTypes"] == call_data[callee_name]["ArgTypes"]:
                            logging.info(f"Call {normalized_callee} from {normalized_fname} : OK")
                        elif len(callinfo["ArgTypes"]) != len(call_data[callee_name]["ArgTypes"]):
                            logging.error(f"Call {normalized_callee} from {normalized_fname} : Numbers of arguments do not match")
                        else:
                            #prnt both argument types
                            #logging.warning(f"Call {normalized_callee} from {normalized_fname} : Possible Mismatch")
                            #loop over the arguments and check if the argument types are the same
                            for i, (arg1, arg2) in enumerate(zip(callinfo["ArgTypes"], call_data[callee_name]["ArgTypes"])):
                                arg1 = dimension2_re.sub('?x', arg1)
                                arg2 = dimension2_re.sub('?x', arg2)
                                arg1s = arg1 
                                arg2s = arg2
                                arg1 = dimension_re.sub("?x", arg1)
                                arg2 = dimension_re.sub("?x", arg2)
                                if arg1 == arg2 and arg1s != arg2s:
                                    fortran_arg1 = fortranize_type(arg1s)
                                    fortran_arg2 = fortranize_type(arg2s)
                                    logging.warning(f"DIMENSION MISMATCH {normalized_fname}->{callnum}:{normalized_callee}({call_data[callee_name]['ArgNames'][i]}) {fortran_arg2} != {fortran_arg1}")
                                if arg1 != arg2:
                                    arg2 = fir_array_re.sub("", arg2)
                                    arg2 = trailing_re.sub(">", arg2)
                                    arg1 = fir_array_re.sub("", arg1)
                                    arg1 = trailing_re.sub(">", arg1)
                                    if arg1 != arg2:
                                        fortran_arg1 = fortranize_type(arg1)
                                        fortran_arg2 = fortranize_type(arg2)
                                        logging.critical(f"TYPE MISMATCH {normalized_fname}->{callnum}:{normalized_callee}({call_data[callee_name]['ArgNames'][i]}) {fortran_arg2} != {fortran_arg1}")
                                    else:
                                        if "array" in arg1 and not "array" in arg2:
                                            logging.error(f"SCALAR VS ARRAY {normalized_fname}->{callnum}:{normalized_callee}({call_data[callee_name]['ArgNames'][i]}) {arg2} != {arg1}")
                                        elif "array" in arg2 and not "array" in arg1:
                                            logging.error(f"ARRAY VS SCALAR {normalized_fname}->{callnum}:{normalized_callee}({call_data[callee_name]['ArgNames'][i]}) {arg1} != {arg2}")

                    else:
                        logging.warning(f"callee {callee_name} not found in the call_data")
                        
    # Display the summary
    summary = collector.get_summary()
    #size of summary
    print(f"Size of summary: {len(summary)}")
#    print("SUMMARY:")
#    print(summary)

    # open static_analysis.supp

    #open static_analysis.log
    #filename is ../starter/static_analysis.log
    #         or ../engine/static_analysis.log
    fname = f"{software}_static_analysis.log"
    print(f"Writing summary to {fname}...")
    with open(fname, "w") as f:
        #write the summary to the file
        f.write(summary)

    #checks if fname exists
    print(f"Checking if {fname} exists...")
    if Path(fname).exists():
        print(f"{fname} exists")
    else:
        print(f"{fname} does not exist")

    #open static_analysis.log and static_analysis.supp simultaneously

    list_of_hashes = []
    # dictionnary "hash" : "message"
    new_errors = {}
    #if file exists
    if Path(fname).exists():
        with open(fname, "r") as f:
            for line in f:
                # if there is no "\n" at the end of the line, add it
                if not line.endswith("\n"):
                    line = line + "\n"
                message_hash = hashlib.md5(re.sub(r'\s+', '', line).encode()).hexdigest()
                list_of_hashes.append(message_hash)
                new_errors[message_hash] = line

    #print the size of the list of hashes
    print(f"Number of errors in the log file: {len(list_of_hashes)}")


    old_errors = {}
    list_of_old_hashes = []
    fname = f"{software}_static_analysis.supp"
    with open(fname, "r") as f:
        for line in f:
            if not line.endswith("\n"):
                line = line + "\n"
            message_hash = hashlib.md5(re.sub(r'\s+', '', line).encode()).hexdigest()
            list_of_old_hashes.append(message_hash)
            old_errors[message_hash] = line


    print(f"Number of errors in the suppression file: {len(list_of_old_hashes)}")

    print("\n\n")
    print("=== SUMMARY ===")
    count_new_errors = 0 
    #compare the two lists
    for hash in list_of_hashes:
        if hash not in list_of_old_hashes:
            count_new_errors += 1
            print("New issue found: ", hash)
            print(new_errors[hash])
#       else:
#           print("Issue already known: ")
#           print(new_errors[hash])
#           print(old_errors[hash])

    print("Total number of new issues: ", count_new_errors)