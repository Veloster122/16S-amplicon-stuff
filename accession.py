# Import necessary libraries
import re

# Load mapping file into a dictionary
def load_mapping(file_path):
    mapping = {}
    with open(file_path, 'r') as f:
        for line in f:
            accession, sample_name = line.strip().split()
            mapping[accession] = sample_name
    return mapping

# Replace accession numbers in the input file
def replace_accession_numbers(input_file, output_file, mapping):
    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        header = infile.readline()
        outfile.write(header)
        for line in infile:
            for accession, sample_name in mapping.items():
                line = re.sub(r'\b' + accession + r'\b', sample_name, line)
            outfile.write(line)

# Paths to your files
mapping_file_path = 'mapping_file.txt'
input_file_path = 'alpha-diversity.97.summary'
output_file_path = 'updated_alpha-diversity.summary'

# Execute the replacement
mapping = load_mapping(mapping_file_path)
replace_accession_numbers(input_file_path, output_file_path, mapping)

print("Accession numbers have been replaced with sample names.")
