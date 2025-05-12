import struct
import os
import sys

def convert_disk_image(input_file):
    """
    Converts a disk image by inserting a 16KB image file after the MBR and
    adjusting the LBA start sectors in the partition table.
    The output filename is derived from the input filename.
    """

    insert_image_file = "dos2_16k.img"
    sector_size = 512
    offset_sectors = 32
    insert_image_size = sector_size * offset_sectors

    # --- Derive the output filename ---
    name, ext = os.path.splitext(input_file)
    output_file = f"{name}_dos2_16k{ext}"

    print(f"Input file   : {input_file}")
    print(f"Output file  : {output_file}")
    print(f"Insert image : {insert_image_file}\n")

    # --- Check if the inserted image file exists and has the correct size ---
    if not os.path.exists(insert_image_file):
        print(f"Error: Insert image file '{insert_image_file}' not found.")
        return

    if os.path.getsize(insert_image_file) != insert_image_size:
        print(f"Error: Insert image file '{insert_image_file}' has incorrect size "
              f"({os.path.getsize(insert_image_file)} bytes), expected {insert_image_size} bytes.")
        return

    # --- Read the input disk image ---
    try:
        with open(input_file, 'rb') as f_in:
            mbr_data = f_in.read(sector_size)
            remaining_data = f_in.read()
    except FileNotFoundError:
        print(f"Error: Input file '{input_file}' not found.")
        return
    except Exception as e:
        print(f"Error reading input file: {e}")
        return

    # --- Read the inserted image ---
    with open(insert_image_file, 'rb') as f_insert:
        insert_data = f_insert.read()

    # --- Parse and modify the partition table ---
    partition_table_offset = 446
    num_partitions = 4
    partition_entry_size = 16

    modified_mbr_data = bytearray(mbr_data)

    for i in range(num_partitions):
        entry_offset = partition_table_offset + i * partition_entry_size
        partition_entry = modified_mbr_data[entry_offset : entry_offset + partition_entry_size]

        if partition_entry[4] != 0:  # Check if the partition has a type
            # Extract LBA start sector (bytes 8-11)
            start_sector_bytes = partition_entry[8:12]
            start_sector = struct.unpack('<I', start_sector_bytes)[0]

            # Calculate the new LBA start sector
            new_start_sector = start_sector + offset_sectors  # Partitions are now shifted forward

            # Update the LBA start sector in the partition entry
            modified_mbr_data[entry_offset + 8 : entry_offset + 12] = struct.pack('<I', new_start_sector)

    # --- Create the output disk image ---
    try:
        with open(output_file, 'wb') as f_out:
            f_out.write(modified_mbr_data)
            f_out.write(insert_data)
            f_out.write(remaining_data)
        print(f"Successfully converted '{input_file}' to '{output_file}'")
    except Exception as e:
        print(f"Error writing to output file: {e}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: dsk_dos2.py <input_disk_image>")
        sys.exit(1)

    input_disk_image = sys.argv[1]
    convert_disk_image(input_disk_image)