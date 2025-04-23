#!/bin/bash
#SBATCH --partition=centos7           # Allocation name
#SBATCH --qos=long                    # Queue name
#SBATCH --time=2-00:00:00             # Wall time
#SBATCH --nodes=1                     # Number of nodes
#SBATCH --ntasks-per-node=1           # Number of tasks per node
##SBATCH --cpus-per-task=32            # Number of cores per task (or leave this out if using the whole node exclusively)
##SBATCH --exclusive                   # Request exclusive access to the node
#SBATCH --output=slurm-%j.out-%N      # Output file
#SBATCH --error=slurm-%j.err-%N       # Error file
#SBATCH --job-name=epi2me             # Job name

# Load necessary modules or environment
module load gcc/9.5.0
module load singularity/3.9.0

# Run the pipeline with the test data
nextflow run /lustre/project/taw/share/epi2me/wf-metagenomics/main.nf \
    -profile singularity \
    --fastq /lustre/project/taw/kvigil/ONR/fastq_all_raw \
    --out_dir /lustre/project/taw/kvigil/ONR/epi2me_out \
    --classifier minimap2 \
    --taxonomy /lustre/project/taw/share/reference/NCBI_taxdump \
    --reference /lustre/project/taw/share/reference/viral.1.1.genomic.fna \
    --ref2taxid /lustre/project/taw/share/reference/ref2taxid.targloci.tsv \
    --minimap2_by_reference True \
    --n_taxa_barplot 20 \
    --igv True \
    --include_read_assignments True \
    --min_len 200 \
    --min_read_qual 15 \
    --taxonomic_rank S