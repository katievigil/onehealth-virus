
install.packages("BiocManager")
BiocManager::install("S4Vectors")

options(BioC_mirror = "http://bioconductor.org")
BiocManager::install("S4Vectors")




library(Biostrings)
library(msa)
library(phangorn)
library(ggtree)



install.packages(c("msa", "ape", "phangorn"))





C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/SD_SEAWATER_SAN_DIEGO_2024-02-21.fasta.txt


# Read sequences
seqs <- readDNAStringSet("C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/SD_SEAWATER_SAN_DIEGO_2024-02-21.fasta.txt")

# Multiple sequence alignment
alignment <- msa(seqs)

# Convert alignment to phyDat
phydat <- as.phyDat(alignment)

# Compute distance matrix
dm <- dist.ml(phydat)

# Build neighbor-joining tree
tree <- NJ(dm)



ggtree(tree) + geom_tiplab()







library(msa)
library(ape)
library(phangorn)

# Align your FASTA
alignment <- msa("your_alignment.fasta")

# Convert to phyDat
phydat <- as.phyDat(alignment)

# Distance matrix and tree
dm <- dist.ml(phydat)
tree <- NJ(dm)

# Bootstrap support (optional)
fit <- pml(tree, data = phydat)
bs <- bootstrap.pml(fit, bs = 100)

# Plot
plotBS(tree, bs, p = 50)


