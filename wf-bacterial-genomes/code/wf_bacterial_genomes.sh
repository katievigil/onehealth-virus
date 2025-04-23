#!/bin/bash
#SBATCH -A loni_virus24    ### Allocation name
#SBATCH -p single           ## Queue name
#SBATCH -t 7-00:00:00       ## Wall time (7 days)
#SBATCH -N 1                ## Number of nodes
#SBATCH -n 1                ## Number of tasks
#SBATCH -c 10               ## Number of cores per task
#SBATCH --output=slurm-%j.out-%N
#SBATCH --error=slurm-%j.err-%N
#SBATCH -J epi2me_alignment ## Job name

# Load modules if needed (e.g., Nextflow)

# Set group ownership for all singularity images
chgrp singularity /ddnB/work/kvigil/epi2me_out/store_dir/viral_nucleotide/minimap_output/work/singularity/*.img

# Set group read/write/execute permissions for all singularity images
chmod 760 /ddnB/work/kvigil/epi2me_out/store_dir/viral_nucleotide/minimap_output/work/singularity/*.img

# Run the workflow
nextflow run epi2me-labs/wf-bacterial-genomes \
    --bam /work/kvigil/epi2me_out/store_dir/viral_nucleotide/minimap_output/bams \
    --reference /work/kvigil/epi2me_out/store_dir/viral_nucleotide/minimap_output/vertebrate_viral_genomes.fasta \
    --reference_based_assembly \
    -profile singularity \
    --run_prokka --kingdom Viruses --metagenome \
    --min_read_length 100 \
    --threads 10 \
    --override_basecaller_cfg dna_r10.4.1_e8.2_400bps_sup@v4.3.0 \
    --outdir /work/kvigil/epi2me_out/wf_bacterial_genome_output