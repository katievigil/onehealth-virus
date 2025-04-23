import pandas as pd
import glob
import os

# Set the directory path
dir_path = r"C:\Users\kvigil\OneDrive - Oxford Nanopore Technologies\results\katie\epi2me_out\alignment_statistics\samples_combined"

# Get all CSV files in the directory
csv_files = glob.glob(os.path.join(dir_path, "*.csv"))

# Merge all CSV files while preserving headers only once
df = pd.concat((pd.read_csv(f) for f in csv_files), ignore_index=True)

# Save the merged file
output_path = os.path.join(dir_path, "merged_data.csv")
df.to_csv(output_path, index=False)

print(f"Merged CSV saved to: {output_path}")
