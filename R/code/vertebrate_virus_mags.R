

##use efetch to get all the genomes from my nucleotide alignment accession numbers from my minimap2 alignment file wf-metagenomics
# Remove duplicates and fetch genomes
sort accession_numbers.txt | uniq | while read accession; do
efetch -db nucleotide -id $accession -format fasta > "${accession}.fasta"
done


# Read the CSV file
accession <- read.csv("C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/vertebrate_accession.csv", stringsAsFactors = FALSE)

# Remove duplicates based on the 'accession' column and keep it as a dataframe
accession_unique <- accession[!duplicated(accession$accession), , drop = FALSE]  # Ensures it's a dataframe

# Ensure the column name is 'accession'
colnames(accession_unique) <- c("accession")

# Save the unique accessions to a new CSV file with column name 'accession'
write.csv(accession_unique, "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/vertebrate_accession_unique.csv", row.names = FALSE)

# Save the unique accessions to a .txt file (one accession per line) with no column name in the text file
write(accession_unique$accession, 
      "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/vertebrate_accession_unique.txt")
