import sys
import os
import math

# Constants
NUM_PARTS = 20  # Fixed number of output files
DEFAULT_INPUT_FILE = "infile.txt"
DEFAULT_OUTFILE = "outfile.inc"

# Parse command-line arguments
input_file = DEFAULT_INPUT_FILE
output_file = DEFAULT_OUTFILE

for arg in sys.argv:
    key_value = arg.split('=')
    if len(key_value) == 2:
        if key_value[0] == '-inputfile':
            input_file = key_value[1]
        elif key_value[0] == '-outfile':
            output_file = key_value[1]

# Extract the output directory from the outfile path
output_dir = os.path.dirname(output_file)

# Ensure output directory exists
os.makedirs(output_dir, exist_ok=True)

# Read input file
with open(input_file, "r", encoding="utf-8") as f:
    lines = [line.strip() for line in f.readlines() if line.strip() and not line.startswith("#")]

total_messages = len(lines)

# Compute how many messages per file (distributed evenly)
base_count = total_messages // NUM_PARTS  # Minimum messages per file
extra = total_messages % NUM_PARTS  # Remaining messages to distribute

# Generate the .inc file (Main include)
with open(output_file, "w", encoding="utf-8") as f_inc:
    print(f"Generating {output_file} with {NUM_PARTS} message parts...")
    f_inc.write("      CHARACTER(LEN=ncharline),DIMENSION(:),ALLOCATABLE :: MESSAGESDATA\n")
    f_inc.write(f"      ALLOCATE(MESSAGESDATA({total_messages}))\n\n")
    f_inc.write(f"      SMESSAGESFILE = {total_messages}\n")

    for i in range(NUM_PARTS):
        f_inc.write(f"      CALL message_part{i+1}(MESSAGESDATA,SMESSAGESFILE)\n")

# Generate Fortran subroutine files in the same directory as the .inc file
start_idx = 0
for i in range(NUM_PARTS):
    # Determine number of messages for this file
    count = base_count + (1 if i < extra else 0)
    part_lines = lines[start_idx: start_idx + count]
    start_idx += count  # Update starting index for next file

    part_filename = os.path.join(output_dir, f"message_part{i+1}.txt")

    with open(part_filename, "w", encoding="utf-8") as f_part:
        f_part.write(f"SUBROUTINE message_part{i+1}(MESSAGESDATA, S)\n")
        f_part.write("  USE NAMES_AND_TITLES_MOD , ONLY : NCHARLINE \n")
        f_part.write("  IMPLICIT NONE\n")
        f_part.write("  INTEGER, INTENT(IN) :: S\n")
        f_part.write("  CHARACTER(LEN=ncharline),INTENT(INOUT) :: MESSAGESDATA(S)\n\n")

        for j, message in enumerate(part_lines, start=start_idx - count + 1):
            safe_message = message.replace("'", "''")  # Escape single quotes for Fortran
            f_part.write(f"  MESSAGESDATA({j}) = '{safe_message}'\n")

        f_part.write("\nEND SUBROUTINE\n")

    print(f"Generated {part_filename} with {count} messages.")

print(f"\nAll {NUM_PARTS} message part files have been created in '{output_dir}'!")
