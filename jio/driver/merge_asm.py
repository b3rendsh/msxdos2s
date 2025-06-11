import re
import os
import sys

def process_includes(input_file, output_file):
    """
    Processes include directives in a Z80 assembly file, not handling nested includes.

    Args:
        input_file (str): The path to the input Z80 assembly file.
        output_file (str): The path to the output Z80 assembly file.
    """
    output_lines = []

    def resolve_path(filename, current_dir):
        """Resolves the full path of the included file."""
        if os.path.isabs(filename):
            return filename
        else:
            return os.path.join(current_dir, filename)

    try:
        with open(input_file, 'r') as infile:
            lines = infile.readlines()
    except FileNotFoundError:
        print(f"Error: File '{input_file}' not found.")
        sys.exit(1)
  
    current_dir = os.path.dirname(input_file)

    for line in lines:
        include_match = re.match(r'^\s*INCLUDE\s+"([^"]+)"', line, re.IGNORECASE)
        if include_match:
            include_file = include_match.group(1)
            include_file_path = resolve_path(include_file, current_dir)
            if os.path.exists(include_file_path):
                output_lines.append(f"\n")
                output_lines.append(f"; ----------------------------------------\n")
                output_lines.append(f"; Included file '{include_file}'\n")
                output_lines.append(f"; ----------------------------------------\n")
                includefile = open(include_file, 'r')
                includelines = includefile.readlines()
                for inlines in includelines:
                    output_lines.append(inlines)
            else:
                output_lines.append(line) 
        else:
            output_lines.append(line)

    try:
        with open(output_file, 'w') as outfile:
            outfile.writelines(output_lines)
        print(f"Successfully merged files into '{output_file}'")
    except Exception as e:
        print(f"Error writing to output file: {e}")
        sys.exit(1)

def main():
    """
    Main function to handle command-line arguments and call process_includes.
    """
    if len(sys.argv) != 3:
        print("Usage: python merge_asm.py <input_file> <output_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    process_includes(input_file, output_file)

if __name__ == "__main__":
    main()
