#!/bin/bash

# Define the directory path (convert Windows path to WSL Linux path)
DIR="/mnt/c/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/alignment_statistics/samples_combined"

# Define the output file
OUTPUT_FILE="$DIR/merged_data.csv"

# Find all CSV files
CSV_FILES=("$DIR"/*.csv)

# Check if CSV files exist
if [ ${#CSV_FILES[@]} -eq 0 ]; then
    echo "No CSV files found in $DIR"
    exit 1
fi

# Extract the header from the first CSV file and write it to output
head -n 1 "${CSV_FILES[0]}" > "$OUTPUT_FILE"

# Loop through all CSV files, skip the header (first line), and append the rest to the output file
for file in "${CSV_FILES[@]}"; do
    tail -n +2 "$file" >> "$OUTPUT_FILE"
done

echo "Merged CSV saved to: $OUTPUT_FILE"
