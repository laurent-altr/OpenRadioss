import sys
import os

# Constants
MESSAGES_PER_FILE = 500 # Split messages every 5000 entries (adjustable)
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

# Determine number of parts
num_parts = (len(lines) + MESSAGES_PER_FILE - 1) // MESSAGES_PER_FILE

# Generate the .inc file (Main include)
with open(output_file, "w", encoding="utf-8") as f_inc:
    print(f"Generating {output_file} with {num_parts} message parts...")
    f_inc.write("      CHARACTER(LEN=ncharline),DIMENSION(:),ALLOCATABLE :: MESSAGESDATA\n")
    f_inc.write(f"      ALLOCATE(MESSAGESDATA({len(lines)}))\n\n")
    f_inc.write(f"      SMESSAGESFILE = {len(lines)}\n")
    f_inc.write("      write(6,*) 'Number of messages in the file: ', SMESSAGESFILE\n\n")
    f_inc.write("      write(6,*) LOC(MESSAGESDATA) \n\n")


    for i in range(num_parts):
        f_inc.write(f"      CALL message_part{i+1}(MESSAGESDATA,SMESSAGESFILE)\n")

#check for open failure


#check if output_file exists
#if it exists, print its content
    if os.path.exists(output_file):
        with open(output_file, "r", encoding="utf-8") as f:
            print(f.read())
    else:
        print("\n \n FILE DOES NOT EXIST")



print(f"Generated {output_file} with {num_parts} message parts.")
#prints the content of output_file
try:
    with open(output_file, "r", encoding="utf-8") as f:
        print(f.read())
except:
    print("File does not exist")

# Generate Fortran subroutine files in the same directory as the .inc file
for i in range(num_parts):
    part_lines = lines[i * MESSAGES_PER_FILE: (i + 1) * MESSAGES_PER_FILE]
    part_filename = os.path.join(output_dir, f"message_part{i+1}.txt")

    with open(part_filename, "w", encoding="utf-8") as f_part:
        f_part.write(f"SUBROUTINE message_part{i+1}(MESSAGESDATA, S)\n")
        f_part.write("  USE NAMES_AND_TITLES_MOD , ONLY : NCHARLINE \n")
        f_part.write("  IMPLICIT NONE\n")
        f_part.write("  INTEGER, INTENT(IN) :: S\n")
        f_part.write("  CHARACTER(LEN=ncharline),INTENT(INOUT) :: MESSAGESDATA(S)\n\n")

        for j, message in enumerate(part_lines, start=1 + i * MESSAGES_PER_FILE):
            safe_message = message.replace("'", "''")  # Escape single quotes for Fortran
            f_part.write(f"  MESSAGESDATA({j}) = '{safe_message}'\n")

        f_part.write("\nEND SUBROUTINE\n")

    print(f"Generated {part_filename} with {len(part_lines)} messages.")

print(f"\nAll {num_parts} message part files have been created in '{output_dir}'!")
