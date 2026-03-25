# Metagenomic characterization of viral communities across coastal matrices highlights bivalves as integrative sentinel hosts within a One Health framework

**Katie Vigil¹, Tiong Gim Aw¹\***

¹Department of Environmental Health Sciences, School of Public Health and Tropical Medicine, Tulane University, New Orleans, LA, USA  

**Correspondence:** Tiong Gim Aw (taw@tulane.edu)

**Keywords:** nanopore sequencing, metagenomics, viruses, bivalve, sentinel species, One Health

---

## Abstract

Metagenomic sequencing enables characterization of viral communities across interconnected environmental and animal hosts within a One Health framework. In coastal ecosystems, sentinel species and environmental matrices may reflect local viral populations. Here, we characterized viral communities across environmental water, bivalves, and marine mammals from two coastal ecosystems to evaluate their potential for integrative viral surveillance. 

Long-read nanopore sequencing was applied to 523 samples from Barataria Bay, Louisiana, and San Diego Bay, California, generating over 282 million reads and identifying 198,507 viral operational taxonomic units (vOTUs). Viral richness differed significantly among sample types (Kruskal–Wallis χ² = 46.67, p = 6.48 × 10⁻⁸), with environmental water exhibiting the highest diversity and marine mammal serum the lowest. 

Viral community composition varied by matrix, with bacteriophages dominating fecal samples, while environmental water and bivalve samples contained diverse viral assemblages largely composed of sequences with unknown host associations alongside mixed viral types. Non-metric multidimensional scaling (NMDS) revealed significant structuring by sample type (PERMANOVA R² = 0.182, p = 0.001), with bivalve-associated communities overlapping with marine mammal viromes, suggesting shared environmental viral signals. 

These results provide a baseline characterization of viral diversity across coastal matrices and demonstrate that environmental water and bivalves capture complementary viral signatures. While this study does not directly assess surveillance performance, it highlights the potential of these matrices for non-invasive, integrative viral monitoring within a One Health framework.

---

## Repository Description

This repository contains code and supporting files for metagenomic analysis of viral communities from:

- Mussels  
- Oysters  
- Seawater  
- Marine mammals  

---

## Data Availability

All raw and processed datasets are available at Zenodo:

https://doi.org/10.5281/zenodo.19211682

---

## Repository Contents

- `R/code/hecatomb_out.R` — main analysis pipeline  
- `allsamples.csv` — sample metadata  
- `quast_results/` — assembly quality metrics  

---

## Reproducibility

To reproduce the analysis:

1. Download data from Zenodo  
2. Place files in a local `data/` directory  
3. Run:
