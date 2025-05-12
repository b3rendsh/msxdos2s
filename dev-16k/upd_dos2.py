import struct
import os
import sys

def update_disk_image(input_file):
    """
    Update a disk image by replacing a 16KB image file after the MBR.
    The output filename is derived from the input filename.
    """

    replace_image_file = "dos2_16k.img"
    sector_size = 512
    offset_sectors = 32
    replace_image_size = sector_size * offset_sectors

    # --- Derive the output filename ---
    name, ext = os.path.splitext(input_file)
    output_file = f"{name}_upd{ext}"

    print(f"Input file    : {input_file}")
    print(f"Output file   : {output_file}")
    print(f"Replace image : {replace_image_file}\n")

    # --- Check if the replace image file exists and has the correct size ---
    if not os.path.exists(replace_image_file):
        print(f"Error: Replace image file '{replace_image_file}' not found.")
        return

    if os.path.getsize(replace_image_file) != replace_image_size:
        print(f"Error: Replace image file '{replace_image_file}' has incorrect size "
              f"({os.path.getsize(replace_image_file)} bytes), expected {replace_image_size} bytes.")
        return

    # --- Read the input disk image ---
    try:
        with open(input_file, 'rb') as f_in:
            mbr_data = f_in.read(sector_size)
            replace_data = f_in.read(replace_image_size)
            remaining_data = f_in.read()
    except FileNotFoundError:
        print(f"Error: Input file '{input_file}' not found.")
        return
    except Exception as e:
        print(f"Error reading input file: {e}")
        return

    # --- Read the inserted image ---
    with open(replace_image_file, 'rb') as f_replace:
        replace_data = f_replace.read()

    # --- Create the output disk image ---
    try:
        with open(output_file, 'wb') as f_out:
            f_out.write(mbr_data)
            f_out.write(replace_data)
            f_out.write(remaining_data)
        print(f"Successfully updated '{input_file}' to '{output_file}'")
    except Exception as e:
        print(f"Error writing to output file: {e}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: upd_dos2.py <input_disk_image>")
        sys.exit(1)

    input_disk_image = sys.argv[1]
    update_disk_image(input_disk_image)